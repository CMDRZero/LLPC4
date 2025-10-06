const std = @import("std");

const Ast = @import("Ast.zig");
const Node = Ast.Expr;
const Tokenizer = @import("Tokenizer.zig");
const Token = Tokenizer.Token;
const types = @import("types.zig");
const Type = types.Type;
const Trie = @import("Trie.zig").Trie;

const SymbolTable = @import("SymbolTable.zig").SymbolTable;

const int = std.math.big.int;

const Parse = @This();
gpa: std.mem.Allocator,
arena_: *std.heap.ArenaAllocator = undefined,
arena: std.mem.Allocator = undefined,
writer: *std.io.Writer,
sourceFile: []const u8,
qerror: ?Error,

censureIsError: bool = true,
warningIsError: bool = false,

tokenStream: Tokenizer.TokenList = undefined,

symboltable: SymbolTable = undefined,
trie: Trie = undefined,

const ParseError = error {
    OutOfMemory,
    compilation_failure,
    Duplicate,
    NoSpaceLeft,
    NegativeIntoUnsigned,
    TargetTooSmall,
    unimplemented,
};

pub fn compilerError(self: *Parse, comptime format: [] const u8, args: anytype) noreturn {
    std.debug.print("INTERNAL ERROR: "++format, args);
    std.debug.print("\nPRINTING LAST TOKEN\n", .{});

    var allocWriter: std.io.Writer.Allocating = .init(std.heap.smp_allocator);
    self.writer = &allocWriter.writer;

    self.newAnnot(.error_, "Current Token at Crash") catch {};
    self.tokenStream.retreat();
    self.underlineSegment(self.tokenStream.current().?.backingstr, .{}) catch {};
    self.throwAnnot() catch {};
    self.printDeinitError();
    std.debug.print("{s}\n", .{allocWriter.written()});
    @panic("INTERNAL ERROR");
}

pub fn init(gpa: std.mem.Allocator, writer: *std.io.Writer, sourceFile: []const u8, defaults: struct { censureIsError: bool = true, warningIsError: bool = false }) !Parse {
    const arenaPtr = try gpa.create(std.heap.ArenaAllocator);
    errdefer gpa.destroy(arenaPtr);
    arenaPtr.* = .init(gpa);
    var self: Parse = .{
        .gpa = gpa,
        .arena = arenaPtr.allocator(),
        .arena_ = arenaPtr,
        .writer = writer,
        .sourceFile = sourceFile,
        .qerror = null,
        .censureIsError = defaults.censureIsError,
        .warningIsError = defaults.warningIsError,
        .trie = .init(gpa),
        .symboltable = .init(arenaPtr.allocator()),
    };
    try self.initError();
    errdefer self.deinitError();
    errdefer self.printError();
    self.tokenStream = try .fromParseFile(&self);
    return self;
}

pub fn deinit(self: *Parse) void {
    self.tokenStream.deinit(self.gpa);
    self.symboltable.deinit();
    self.trie.deinit(self.gpa);
    self.arena_.deinit();
    self.gpa.destroy(self.arena_);
}

pub const MessageType = enum {
    warning_,
    censure_,
    error_,
};

pub fn throw(self: Parse, kind: MessageType) !void {
    switch (kind) {
        .warning_ => if (self.warningIsError) return error.compilation_failure else {},
        .censure_ => if (self.censureIsError) return error.compilation_failure else {},
        .error_ => return error.compilation_failure,
    }
}

pub const Error = struct {
    annots: []Annotation,

    pub const Annotation = struct {
        msg: []const u8,
        kind: MessageType,
        marks: []Mark,

        pub const Mark = struct {
            mark: u8,
            line: usize,
            colStart: usize,
            len: usize,
        };
    };
};

pub fn printError(self: Parse) void {
    if (self.qerror == null) return;
    const err = self.qerror.?;
    const totLines = std.mem.count(u8, self.sourceFile, "\n") + 1;
    const digitsForLine: u8 = @intCast(1 + std.math.log10(@max(0, totLines)));

    for (err.annots) |annot| {
        var lines: std.ArrayList(usize) = .empty;
        defer lines.deinit(self.gpa);

        for (annot.marks) |mark| {
            for (lines.items) |line| {
                if (mark.line == line) break;
            } else {
                lines.append(self.gpa, mark.line) catch unreachable;
            }
        }

        std.mem.sort(usize, lines.items, {}, struct {
            fn lt(_: void, a: usize, b: usize) bool {
                return a < b;
            }
        }.lt);

        for (0..digitsForLine) |_| self.writer.print(" ", .{}) catch {};
        self.writer.print(" |\n", .{}) catch {};
        var prevline: usize = lines.items[0];
        for (lines.items) |line| {
            defer prevline = line;
            if (line > prevline + 2) {
                for (0..digitsForLine) |_| self.writer.print(" ", .{}) catch {};
                self.writer.print("...\n", .{}) catch {};
            } else if (line == prevline + 2) {
                switch (digitsForLine) {
                    inline 1...20 => |x| {
                        const msg = comptime std.fmt.comptimePrint("{{: >{}}} |    {{s}}\n", .{x});
                        const strline = self.getLine(prevline + 1);
                        self.writer.print(msg, .{ line - 1, std.mem.trimStart(u8, strline, " \t") }) catch {};
                    },
                    else => unreachable,
                }
            }
            switch (digitsForLine) {
                inline 1...20 => |x| {
                    const msg = comptime std.fmt.comptimePrint("{{: >{}}} |    {{s}}\n", .{x});
                    const strline = self.getLine(line);
                    self.writer.print(msg, .{ line, std.mem.trimStart(u8, strline, " \t") }) catch {};
                },
                else => unreachable,
            }
            var marks = " ".* ** 128;
            for (annot.marks) |mark| {
                if (mark.line != line) continue;
                for (mark.colStart..mark.colStart + mark.len) |idx| marks[idx] = mark.mark;
            }
            var strlen: usize = marks.len;
            while (strlen > 0) {
                if (marks[strlen - 1] != ' ') break;
                strlen -= 1;
            }
            for (0..digitsForLine) |_| self.writer.print(" ", .{}) catch {};
            self.writer.print(" |    {s}\n", .{marks[0..strlen]}) catch {};
        }
        const tagname = @tagName(annot.kind);
        var buf: [256]u8 = undefined;
        const prefix = std.ascii.upperString(&buf, tagname);
        self.writer.print("{s}: {s}\n\n", .{ prefix[0 .. prefix.len - 1], annot.msg }) catch {};
    }
}

pub fn initError(self: *Parse) !void {
    const annots = try self.gpa.alloc(Error.Annotation, 0);
    self.qerror = .{
        .annots = annots,
    };
}

pub fn printDeinitError(self: *Parse) void {
    self.printError();
    self.deinitError();
}

pub fn deinitError(self: *Parse) void {
    if (self.qerror) |*eptr| {
        for (eptr.annots) |annot| {
            self.gpa.free(annot.marks);
        }
        self.gpa.free(eptr.annots);
        self.qerror = null;
    }
}

pub fn newAnnot(self: *Parse, kind: MessageType, msg: []const u8) !void {
    const marks = try self.gpa.alloc(Error.Annotation.Mark, 0);
    const oldannots = self.qerror.?.annots;
    defer self.gpa.free(oldannots);
    const newannots = try self.gpa.alloc(Error.Annotation, oldannots.len + 1);
    @memcpy(newannots.ptr, oldannots);
    newannots[oldannots.len] = .{
        .msg = msg,
        .kind = kind,
        .marks = marks,
    };
    self.qerror = .{ .annots = newannots };
}

pub fn throwAnnot(self: Parse) !void {
    const annots = self.qerror.?.annots;
    const kind = annots[annots.len - 1].kind;
    return self.throw(kind);
}

fn newMark(self: *Parse, mark: Error.Annotation.Mark) !void {
    const annots = self.qerror.?.annots;
    const oldmarks = annots[annots.len - 1].marks;
    defer self.gpa.free(oldmarks);
    const newmarks = try self.gpa.alloc(Error.Annotation.Mark, oldmarks.len + 1);
    @memcpy(newmarks.ptr, oldmarks);
    newmarks[oldmarks.len] = mark;
    self.qerror.?.annots[annots.len - 1].marks = newmarks;
}

/// Assumes `str` is a subslice of `self.sourceFile`
pub fn underlineSegment(self: *Parse, str: []const u8, defaults: struct { mark: u8 = '-' }) !void {
    var ustr = str;
    const srcline, const lineno = self.getLineStartOf(str);
    const ltrim = srcline.len - std.mem.trimStart(u8, srcline, " \t").len;
    for (str, 0..) |c, i| {
        if (c == '\n') {
            try self.underlineSegment(std.mem.trimStart(u8, str[i + 1 ..], " \t"), defaults);
            ustr = str[0..i];
            break;
        }
    }
    try self.newMark(.{ .mark = defaults.mark, .colStart = ustr.ptr - srcline.ptr - ltrim, .len = ustr.len, .line = lineno });
}

fn getLineStartOf(self: Parse, line: []const u8) struct { []const u8, usize } {
    const byteoffset = line.ptr - self.sourceFile.ptr;
    var lineno: usize = 0;
    var lastlinestart: usize = 0;
    for (self.sourceFile[0..byteoffset], 0..) |c, i| {
        if (c == '\n') {
            lastlinestart = i + 1;
            lineno += 1;
        }
    }
    var srcline = self.sourceFile[lastlinestart..];
    for (srcline, 0..) |c, len| {
        if (c == '\n') return .{ srcline[0..len], lineno };
    } else return .{ srcline, lineno };
}

fn getLine(self: Parse, line: usize) []const u8 {
    var count: usize = 0;
    for (self.sourceFile, 0..) |c, idx| {
        if (count == line) {
            if (c == '\r') continue;
            var slice = self.sourceFile[idx..];
            for (slice, 0..) |c2, len| {
                if (c2 == '\n') return slice[0..len];
            } else return slice;
        }
        if (c == '\n') {
            count += 1;
        }
    } else unreachable;
}

pub fn nextToken(self: *Parse) !Token {
    return Tokenizer.readToken(self.untokenStream, self.prevToken);
}

pub fn isDefered(self: *Parse, token: Token) !?SymbolTable.Reference {
    if (token.tag == .@"infer") {
        self.tokenStream.consume();
        const tkn = try self.someToken();
        try self.expectToken(._identifier);
        const string = try std.mem.concat(self.arena, u8, &[2][]const u8{"infer ", tkn.backingstr});
        const str = try self.trie.writeGet(string);
        //self.gpa.free(string);
        return SymbolTable.refName(str);
    }
    if (token.tag == ._identifier) {
        self.tokenStream.consume();
        const str = try self.trie.writeGet(token.backingstr);
        return SymbolTable.refName(str);
    }
    return null;
}

pub fn refToIdx(self: *Parse, ref: SymbolTable.Reference) !SymbolTable.Idx {
    return self.symboltable.toIdx(ref) catch {
        try self.newAnnot(.error_, "Undefined Reference");
        try self.underlineSegment(self.trie.getString(ref.name.name).?, .{.mark = '^'});
        try self.throwAnnot();
        unreachable;
    };
}

const Assoc = enum {
    left,
    none,
};

const PrecClass = struct {
    major: Major,
    minor: u8 = 0,
    assoc: Assoc = Assoc.left,

    const Major = enum(u3) {
        arithmetic = 0,
        bitwise = 1,
        comparison = 2,
        logical = 3,
        coercion = 4,
        root = 5,
    };

    pub const start: PrecClass = .{ .major = .root };

    pub const Rel = enum(i2) {
        lt = -1,
        eq = 0,
        gt = 1,

        pub fn inverted(self: @This()) @This() {
            return @enumFromInt(-@intFromEnum(self));
        }
    };

    /// arith > coerc
    /// bitwi > coerc
    /// coerc > compr > logic > root
    /// order[y][x] --> y cmp x
    const major_ordering = b: {
        const base_greater_than_relations = [_][2]Major{
            .{ .arithmetic, .coercion },
            .{ .bitwise, .coercion },
            .{ .coercion, .comparison },
            .{ .comparison, .logical },
            .{ .logical, .root },
        };

        var order: [6][6]?Rel = @splat(@splat(null));
        for (base_greater_than_relations) |relation| {
            const i = @intFromEnum(relation[0]);
            const j = @intFromEnum(relation[1]);
            order[i][j] = .gt;
        }

        computeTransativeOrdering(&order);
        break :b order;
    };

    pub fn cmp(lhs: PrecClass, rhs: PrecClass) ?Rel {
        return lhs._cmp(rhs) orelse (rhs._cmp(lhs) orelse return null).inverted();
    }

    fn _cmp(lhs: PrecClass, rhs: PrecClass) ?Rel {
        if (std.meta.eql(lhs, rhs)) return .eq;
        if (lhs.major != rhs.major) {
            return PrecClass.major_ordering[@intFromEnum(lhs.major)][@intFromEnum(rhs.major)];
        } else if (lhs.major == .arithmetic) {
            if (lhs.minor == 0 and rhs.minor == 2) return .gt;
            if (lhs.minor == 1 and rhs.minor == 2) return .gt;
            if (lhs.minor == 0 and rhs.minor == 1) return .eq;
        } else if (lhs.major == .bitwise) {
            if (lhs.minor == 0 and rhs.minor == 1) return .gt;
            if (lhs.minor == 0 and rhs.minor == 3) return .gt;
            if (lhs.minor == 1 and rhs.minor == 3) return .gt;
        } else if (lhs.major == .logical) {
            if (lhs.minor == 0 and rhs.minor == 1) return .gt;
        }
        return null;
    }

    fn computeTransativeOrdering(order: *[6][6]?Rel) void {
        for (0..6) |i| for (0..6) |j| for (0..6) |k| {
            if (order[i][k] == order[k][j] and order[i][k] != null) order[i][j] = order[i][k];
        };
        for (0..6) |i| for (0..6) |j| {
            if (i == j) order[i][j] = .eq;
            if (order[i][j] == null and order[j][i] != null) order[i][j] = order[j][i].?.inverted();
        };
    }
};

const OperInfo = struct {
    prec: PrecClass,
};

// A table of binary operator information. Operator classes are as follows:
//  arith 0:                 * *% *| /
//  arith 1 nonchainable:    %
//  arith 2:                 + - +% -% +| -|
//  bitwi 0:                 << <<% <<| <<< >> >>>
//  bitwi 1:                 & &~
//  bitwi 2:                 ^ ^~
//  bitwi 3:                 | |~
//  coerc:                   orelse catch
//  compr nonchainable:      == != < <= >= >
//  logic 0:                 and
//  logic 1:                 or
// Class Ordering is this:
//  arith > coerc
//  bitwi > coerc
//  coerc > compr > logic
// With subclass order of this:
//  a0 == a1 > a2
//  b0 > b1 > b3
//  l0 > l1
const operTable = std.enums.directEnumArrayDefault(Token.Tag, OperInfo, null, 0, .{
    .@"or" = .{ .prec = .{ .major = .logical, .minor = 1 } },

    .@"and" = .{ .prec = .{ .major = .logical, .minor = 0 } },

    .@"==" = .{ .prec = .{ .major = .comparison, .assoc = Assoc.none } },
    .@"!=" = .{ .prec = .{ .major = .comparison, .assoc = Assoc.none } },
    .@"<" = .{ .prec = .{ .major = .comparison, .assoc = Assoc.none } },
    .@">" = .{ .prec = .{ .major = .comparison, .assoc = Assoc.none } },
    .@"<=" = .{ .prec = .{ .major = .comparison, .assoc = Assoc.none } },
    .@">=" = .{ .prec = .{ .major = .comparison, .assoc = Assoc.none } },

    .@"&" = .{ .prec = .{ .major = .bitwise, .minor = 1 } },
    .@"&~" = .{ .prec = .{ .major = .bitwise, .minor = 1 } },
    .@"^" = .{ .prec = .{ .major = .bitwise, .minor = 2 } },
    .@"^~" = .{ .prec = .{ .major = .bitwise, .minor = 2 } },
    .@"|" = .{ .prec = .{ .major = .bitwise, .minor = 3 } },
    .@"|~" = .{ .prec = .{ .major = .bitwise, .minor = 3 } },

    .@"orelse" = .{ .prec = .{ .major = .coercion } },
    .@"catch" = .{ .prec = .{ .major = .coercion } },

    .@"<<" = .{ .prec = .{ .major = .bitwise, .minor = 0 } },
    .@"<<%" = .{ .prec = .{ .major = .bitwise, .minor = 0 } },
    .@"<<|" = .{ .prec = .{ .major = .bitwise, .minor = 0 } },
    .@"<<<" = .{ .prec = .{ .major = .bitwise, .minor = 0 } },
    .@">>" = .{ .prec = .{ .major = .bitwise, .minor = 0 } },
    .@">>>" = .{ .prec = .{ .major = .bitwise, .minor = 0 } },

    .@"+" = .{ .prec = .{ .major = .arithmetic, .minor = 2 } },
    .@"+%" = .{ .prec = .{ .major = .arithmetic, .minor = 2 } },
    .@"+|" = .{ .prec = .{ .major = .arithmetic, .minor = 2 } },
    .@"-" = .{ .prec = .{ .major = .arithmetic, .minor = 2 } },
    .@"-%" = .{ .prec = .{ .major = .arithmetic, .minor = 2 } },
    .@"-|" = .{ .prec = .{ .major = .arithmetic, .minor = 2 } },

    .@"*" = .{ .prec = .{ .major = .arithmetic, .minor = 0 } },
    .@"*%" = .{ .prec = .{ .major = .arithmetic, .minor = 0 } },
    .@"*|" = .{ .prec = .{ .major = .arithmetic, .minor = 0 } },
    .@"/" = .{ .prec = .{ .major = .arithmetic, .minor = 0 } },

    .@"%" = .{ .prec = .{ .major = .arithmetic, .minor = 1, .assoc = Assoc.none } },
});

// fn parseExprPrecedence(p: *Parse, token: Token, min_exc_prec: PrecClass) ParseError!?Node {
//     var node = try p.parsePrefixExpr(token) orelse return null;

//     while (true) {
//         const tok_tag = p.tokenTag(p.tok_i);
//         const info = operTable[@as(usize, @intCast(@intFromEnum(tok_tag)))];
//         const rel = info.prec.cmp(min_exc_prec) orelse return p.fail(.ambiguous_operator_precedence);

//         if (rel == .lt) {
//             break;
//         }

//         if (min_exc_prec.major == info.prec.major and min_exc_prec.assoc == .none and info.prec.assoc == .none) {
//             if (info.prec.major == .comparison) {
//                 return p.fail(.chained_comparison_operators);
//             } else {
//                 return p.fail(.illegal_chained_operators);
//             }
//         }

//         if (rel == .eq) {
//             break;
//         }

//         const oper_token = p.nextToken();
//         // Special-case handling for "catch"
//         // if (tok_tag == .keyword_catch) {
//         //     _ = try p.parsePayload();
//         // }
//         const rhs = try p.parseExprPrecedence(info.prec) orelse {
//             try p.warn(.expected_expr);
//             return node;
//         };

//         {
//             const tok_len = tok_tag.lexeme().?.len;
//             const char_before = p.source[p.tokenStart(oper_token) - 1];
//             const char_after = p.source[p.tokenStart(oper_token) + tok_len];
//             if (tok_tag == .ampersand and char_after == '&') {
//                 // without types we don't know if '&&' was intended as 'bitwise_and address_of', or a c-style logical_and
//                 // The best the parser can do is recommend changing it to 'and' or ' & &'
//                 try p.warnMsg(.{ .tag = .invalid_ampersand_ampersand, .token = oper_token });
//             } else if (std.ascii.isWhitespace(char_before) != std.ascii.isWhitespace(char_after)) {
//                 try p.warnMsg(.{ .tag = .mismatched_binary_op_whitespace, .token = oper_token });
//             }
//         }

//         node = try p.addNode(.{
//             .tag = info.tag,
//             .main_token = oper_token,
//             .data = .{ .node_and_node = .{ node, rhs } },
//         });
//     }

//     return node;
// }

// fn parsePrefixExpr(p: *Parse, token: Token) ParseError!?Node {
//     _ = switch (token.tag) {
//         .@"!", .@"~", .@"try", .@"-", .@"-%" => {},
//         else => return p.parsePrimaryExpr(token),
//     };
//     const rhsptr = try p.gpa.create(Node);
//     const nexttkn = p.nextToken();
//     rhsptr.* = try p.expectPrefixExpr(nexttkn);
//     return .{
//         .main_token = nexttkn,
//         .data = .{
//             .op_suffix_unary = .{
//                 .result_type = null,
//                 .rhs = rhsptr,
//             }
//         }
//     };
// }

// fn parseExpr(p: *Parse) ParseError!?Node {
//     return p.parseExprPrecedence(.start);
// }

const z85str = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWYZ.-:+=^!/*?&<>()[]{}@%$#";
pub fn z85DigitToValue(digit: u8) u32 {
    const lookup = b: {
        var ary: [128] u8 = @splat(255);
        for (z85str, 0..) |char, val| {
            ary[char] = @intCast(val);
        }
        break :b ary;
    };

    const val = lookup[digit];
    if (val == 255) {
        @branchHint(.cold);
        std.debug.panic("Got bad digit `{c}` in z85 decoding.\n", .{digit});
    }
    return val;
}

///Cannot Error, our Tokenizer is responsible for that
pub fn computeParsedInteger(alloc: std.mem.Allocator, string: [] const u8) !int.Managed {
    const State = enum {
        start,
        zero_prefix,
        digit,
        z85_value,
    };

    var cumulative: int.Managed = try .initSet(alloc, 0);
    errdefer cumulative.deinit();
    var idx: usize = 0;
    var base: int.Managed = try .init(alloc);
    defer base.deinit();

    sw: switch (State.start) {
        .start => switch (string[idx]) {
            '0' => {
                idx += 1;
                if (string.len == 1) break :sw;
                continue :sw .zero_prefix;
            },
            '1'...'9' => {
                try base.set(10);
                continue :sw .digit;
            },
            'z' => {
                try base.set(85);
                idx += 2;
                continue :sw .z85_value;
            },
            else => unreachable,
        },
        .zero_prefix => switch (string[idx]) {
            '0'...'9' => {
                try base.set(10);
                continue :sw .digit;
            },
            'x' => {
                try base.set(16);
                idx += 1;
                continue :sw .digit;
            },
            'b' => {
                try base.set(2);
                idx += 1;
                continue :sw .digit;
            },
            'd' => {
                try base.set(10);
                idx += 1;
                continue :sw .digit;
            },
            'o' => {
                try base.set(8);
                idx += 1;
                continue :sw .digit;
            },
            else => unreachable,
        },
        .digit => {
            switch (string[idx]) {
                '_' => {},
                '0'...'9' => |char| {
                    try cumulative.mul(&cumulative, &base);
                    try cumulative.addScalar(&cumulative, char - '0');
                },
                'a'...'f' => |char| {
                    try cumulative.mul(&cumulative, &base);
                    try cumulative.addScalar(&cumulative, 10 + char - 'a');
                },
                'A'...'F' => |char| {
                    try cumulative.mul(&cumulative, &base);
                    try cumulative.addScalar(&cumulative, 10 + char - 'A');
                },
                else => unreachable,
            }
            idx += 1;
            if (idx >= string.len) break: sw;
            continue :sw .digit;
        },
        .z85_value => for (string[idx..]) |c| {
            try cumulative.mul(&cumulative, &base);
            try cumulative.addScalar(&cumulative, z85DigitToValue(c));
        },
    }
    return cumulative;
}

pub fn parseType(self: *Parse) ParseError!?SymbolTable.Reference {
    return try self.parseTypeArgs(.{.access = .mut});
}

fn parseTypeArgs(self: *Parse, default: struct {access: Type.AccessQualifier = .view}) ParseError!?SymbolTable.Reference {
    if (try self.isDefered(try self.someToken())) |r| return r;
    const qdata = try self.parseDataQualifier();
    const qaccess = try self.parseAccessQualifier();
    const alignment = try self.parseAlign();
    if (qdata == null and qaccess == null and alignment == null) return null;
    const aggr = try self.expectAggregateType();

    const data = qdata orelse .@"var";
    const access = qaccess orelse default.access;

    return try self.symboltable.addValueAndRef(Type {
        .data = data,
        .access = access,
        .tweaks = .{ .alignment = alignment },
        .aggregate = aggr,
    });
}

fn expectAggregateType(self: *Parse) ParseError!SymbolTable.Reference {
    return try self.parseAggregateType() orelse {
        const tkn = try self.someToken();
        try self.newAnnot(.error_, "Expected Aggregate Type");
        try self.underlineSegment(tkn.backingstr, .{.mark = '^'});
        try self.throwAnnot();
        unreachable;
    };
}

fn parseAggregateType(self: *Parse) ParseError!?SymbolTable.Reference {
    if (try self.isDefered(try self.someToken())) |r| return r;
    const ret: types.Aggregate = b: {
        if (try self.parseAggregatePrefixed()) |x| break :b .{.prefixed = x};
        if (try self.parseAggregateOrderedTuple()) |x| break :b .{.orderedTuple = x};
        if (try self.parseAggregateFieldNamedTuple()) |x| break :b .{.fieldNamedTuple = x};
        return null;
    };
    return try self.symboltable.addValueAndRef(ret);
}

fn parseAggregatePrefixed(self: *Parse) ParseError!?types.Aggregate.Prefixed {
    const prefix: types.Aggregate.Prefix = try self.parseAggregatePrefix() orelse return null;
    return .{
        .prefix = prefix,
        .aggregate = try self.expectAggregateType(),
    };
}

fn parseAggregatePrefix(self: *Parse) ParseError!?types.Aggregate.Prefix {
    return try self.parseArrayPrefix()
    orelse try self.parseNullPrefix()
    orelse try self.parseErrorPrefix();
}

fn parseArrayPrefix(self: *Parse) ParseError!?types.Aggregate.Prefix {
    _ = self;
    return null; //Unimplemented
}
fn parseNullPrefix(self: *Parse) ParseError!?types.Aggregate.Prefix {
    _ = self;
    return null; //Unimplemented
}
fn parseErrorPrefix(self: *Parse) ParseError!?types.Aggregate.Prefix {
    _ = self;
    return null; //Unimplemented
}

fn parseAggregateOrderedTuple(self: *Parse) ParseError!?types.Aggregate.OrderedTuple {
    _ = self;
    return null; //Unimplemented
}

fn parseAggregateFieldNamedTuple(self: *Parse) ParseError!?types.Aggregate.FieldNamedTuple {
    _ = self;
    return null; //Unimplemented
}

fn parseDataQualifier(self: *Parse) ParseError!?Type.DataQualifier {
    const token: Token = self.tokenStream.current() orelse return null;
    self.tokenStream.consume();
    const tag: Type.DataQualifier = b: {
        if (token.tag == .@"const") break :b .@"const";
        if (token.tag == .@"var") break :b .@"var";
        if (token.tag == .@"volatile") break :b .@"volatile";
        self.tokenStream.retreat();
        return null;
    };
    return tag;
}

fn parseAccessQualifier(self: *Parse) ParseError!?Type.AccessQualifier {
    const token: Token = self.tokenStream.current() orelse return null;
    self.tokenStream.consume();
    const tag: Type.AccessQualifier = b: {
        if (token.tag == .@"view") break :b .@"view";
        if (token.tag == .@"mut") break :b .@"mut";
        self.tokenStream.retreat();
        return null;
    };
    return tag;
}

fn parseAlign(self: *Parse) ParseError!?SymbolTable.Reference {
    const token: Token = self.tokenStream.current() orelse return null;
    if (token.tag == .@"bitAlign") {
        self.tokenStream.consume();
        try self.expectToken(.@"(");
        var ret: SymbolTable.Reference = undefined;
        if (try self.isDefered(try self.someToken())) |r| {
            ret = r;
        } else { 
            const valueRef: SymbolTable.Reference = try self.expectInteger();
            const idx = self.symboltable.toIdx(valueRef) catch self.compilerError("Did not write", .{});
            const value = self.symboltable.getValue(idx, int.Managed) orelse self.compilerError("Incorrectly typed", .{});
            ret = try self.symboltable.addValueAndRef(try value.toInt(usize));
        }
        try self.expectToken(.@")");
        return ret;
    }
    return null;
}

fn expectInteger(self: *Parse) !SymbolTable.Reference {
    return try self.parseInteger() orelse {
        if (self.tokenStream.current()) |tkn| {
            var buf: [128] u8 = @splat(' ');
            try self.newAnnot(.error_, try std.fmt.bufPrint(&buf, "Expected Integer but found `{s}`", .{@tagName(tkn.tag)}));
            try self.underlineSegment(tkn.backingstr, .{.mark = '^'});
            try self.throwAnnot();
        } else {
            try self.newAnnot(.error_, "Expected Integer but found EOF");
            self.tokenStream.retreat();
            try self.underlineSegment(self.tokenStream.current().?.backingstr, .{});
            try self.throwAnnot();
        }
        unreachable;
    };
}

fn expectToken(self: *Parse, comptime token: Token.Tag) !void {
    const tkn = self.tokenStream.current() orelse {
        try self.newAnnot(.error_, "Expected Token but found EOF");
        self.tokenStream.retreat();
        try self.underlineSegment(self.tokenStream.current().?.backingstr, .{});
        try self.throwAnnot();
        unreachable;
    };
    if (tkn.tag == token) {
        self.tokenStream.consume();
        return;
    }
    try self.newAnnot(.error_, std.fmt.comptimePrint("Expected Token {}", .{token}));
    try self.underlineSegment(self.tokenStream.current().?.backingstr, .{});
    try self.throwAnnot();
}

fn someToken(self: *Parse) !Token {
    return self.tokenStream.current() orelse {
        try self.newAnnot(.error_, "Expected Token but found EOF");
        self.tokenStream.retreat();
        try self.underlineSegment(self.tokenStream.current().?.backingstr, .{});
        try self.throwAnnot();
        unreachable;
    };
}

fn parseInteger(self: *Parse) ParseError!?SymbolTable.Reference {
    const tkn = self.tokenStream.current() orelse return null;
    if (try self.isDefered(tkn)) |r| return r;
    if (tkn.tag != ._number) return null;
    self.tokenStream.consume();
    const value =  try computeParsedInteger(self.arena, tkn.backingstr);
    return try self.symboltable.addValueAndRef(value);
}

test "Error using Functions" {
    var allocWriter: std.io.Writer.Allocating = .init(std.testing.allocator);
    defer allocWriter.deinit();

    var p: Parse = .{
        .gpa = std.testing.allocator,
        .writer = &allocWriter.writer,
        .sourceFile =
        \\fn Main() void {
        \\  return 1 + 2 & 3;
        \\}
        ,
        .qerror = null,
    };
    {
        try p.initError();
        defer p.printDeinitError();

        try p.newAnnot(.censure_, "TEST BUT CLEANER");
        try p.underlineSegment(p.sourceFile[26..][0..9], .{});
        try p.underlineSegment(p.sourceFile[26..][2..3], .{ .mark = '^' });
        try p.underlineSegment(p.sourceFile[26..][6..7], .{ .mark = '^' });
        p.throwAnnot() catch {};

        try p.newAnnot(.censure_, "TEST AGAIN");
        try p.underlineSegment(p.sourceFile[0..25], .{ .mark = '~' });
        p.throwAnnot() catch {};

        try p.newAnnot(.warning_, "Split Message");
        try p.underlineSegment(p.sourceFile[0..1], .{ .mark = '*' });
        try p.underlineSegment(p.sourceFile[37..38], .{ .mark = '*' });
        try p.throwAnnot();

        try p.newAnnot(.error_, "MULTIPLE ANNOTATIONS!!!");
        try p.underlineSegment(p.sourceFile[26..], .{ .mark = '^' });
        p.throwAnnot() catch {};
    }
    try std.testing.expectEqualStrings(
\\  |
\\1 |    return 1 + 2 & 3;
\\  |           --^---^--
\\CENSURE: TEST BUT CLEANER
\\
\\  |
\\0 |    fn Main() void {
\\  |    ~~~~~~~~~~~~~~~~
\\1 |    return 1 + 2 & 3;
\\  |    ~~~~~~
\\CENSURE: TEST AGAIN
\\
\\  |
\\0 |    fn Main() void {
\\  |    *
\\1 |    return 1 + 2 & 3;
\\2 |    }
\\  |    *
\\WARNING: Split Message
\\
\\  |
\\1 |    return 1 + 2 & 3;
\\  |           ^^^^^^^^^^
\\2 |    }
\\  |    ^
\\ERROR: MULTIPLE ANNOTATIONS!!!
\\
\\
        , allocWriter.written(),
    );
}

test "Test Tokenization Error" {
    var allocWriter: std.io.Writer.Allocating = .init(std.testing.allocator);
    defer allocWriter.deinit();
    const pOrErr = Parse.init(
        std.testing.allocator,
        &allocWriter.writer,
        \\pub fn Main() !void {
        \\  return 0b0123456789;
        \\}
        , .{}
    );
    try std.testing.expectError(error.compilation_failure, pOrErr);
    
    try std.testing.expectEqualStrings(
        \\  |
        \\1 |    return 0b0123456789;
        \\  |           ----^^^^^^^^
        \\ERROR: Invalid Digit Sequence
        \\
        \\
        , allocWriter.written()
    );
}

test "Type Parse Test" {
    const file = 
        \\const view bitAlign(?) T
    ;

    var allocWriter: std.io.Writer.Allocating = .init(std.testing.allocator);
    defer allocWriter.deinit();
    var p: Parse = try .init(
        std.testing.allocator,
        &allocWriter.writer,
        file,
        .{},
    );
    defer p.deinit();
    
    {
        defer p.printDeinitError();
        _ = p.parseType() catch {};
    }
    
    try std.testing.expectEqualStrings(
        \\  |
        \\0 |    const view bitAlign(?) T
        \\  |                        ^
        \\ERROR: Expected Integer but found `?`
        \\
        \\
        , allocWriter.written()
    );
}

test "Failing BitAlign" {
    const file = 
        \\const view bitAlign(?) T
    ;

    var allocWriter: std.io.Writer.Allocating = .init(std.testing.allocator);
    defer allocWriter.deinit();
    var p: Parse = try .init(
        std.testing.allocator,
        &allocWriter.writer,
        file,
        .{},
    );
    defer p.deinit();
    
    {
        defer p.printDeinitError();
        _ = p.parseType() catch {};
    }
    
    try std.testing.expectEqualStrings(
        \\  |
        \\0 |    const view bitAlign(?) T
        \\  |                        ^
        \\ERROR: Expected Integer but found `?`
        \\
        \\
        , allocWriter.written()
    );
}

test "Succeeding BitAlign" {
    const file = 
        \\const view bitAlign(N) infer T
    ;

    var allocWriter: std.io.Writer.Allocating = .init(std.testing.allocator);
    defer allocWriter.deinit();
    errdefer std.debug.print("{s}\n", .{allocWriter.written()});
    var p: Parse = try .init(
        std.testing.allocator,
        &allocWriter.writer,
        file,
        .{},
    );
    defer p.deinit();
    
    {
        defer p.printDeinitError();
        const typeRef = try p.parseType() orelse return error.Null;
        const typeIdx = try p.symboltable.toIdx(typeRef);
        const kind = p.symboltable.getValue(typeIdx, Type).?;
        _ = kind;
    }

    try std.testing.expectEqualStrings(
        ""
        , allocWriter.written()
    );
}

test "Type Resolution Failure Undef" {
    const file = 
        \\T;
    ;

    var allocWriter: std.io.Writer.Allocating = .init(std.testing.allocator);
    defer allocWriter.deinit();
    errdefer std.debug.print("{s}\n", .{allocWriter.written()});
    var p: Parse = try .init(
        std.testing.allocator,
        &allocWriter.writer,
        file,
        .{},
    );
    defer p.deinit();
    
    {
        defer p.printDeinitError();
        const typeRef = try p.parseType() orelse return error.Null;
        _ = Type.resolve(&p, typeRef) catch {};
    }

    try std.testing.expectEqualStrings(
\\  |
\\0 |    T;
\\  |    ^
\\ERROR: Undefined Reference
\\
\\
        , allocWriter.written()
    );
}

test "Type Resolution Failure Bad Type" {
    const file = 
        \\T;
        \\const T = 128;
    ;

    var allocWriter: std.io.Writer.Allocating = .init(std.testing.allocator);
    defer allocWriter.deinit();
    errdefer std.debug.print("{s}\n", .{allocWriter.written()});
    var p: Parse = try .init(
        std.testing.allocator,
        &allocWriter.writer,
        file,
        .{},
    );
    defer p.deinit();
    
    {
        defer p.printDeinitError();
        const typeRef = try p.parseType() orelse return error.Null;
        const name = try p.trie.writeGet("T");
        try p.symboltable.assignNamedValue(name, @as(usize, 128));
        _ = Type.resolve(&p, typeRef) catch {};
    }

    try std.testing.expectEqualStrings(
\\  |
\\0 |    T;
\\  |    ^
\\ERROR: Expected `Type`, found `usize`
\\
\\
        , allocWriter.written()
    );
}

// test "Precedence" {
//     var p: Parse = .init(std.testing.allocator,
//         \\x + y * z
//     , .{});

//     try p.initError();
//     defer p.printDeinitError();
//     const node = try p.parseExpr();
//     std.debug.print("Node is {}\n", .{node}) catch {};
// }
