const std = @import("std");
const Parse = @import("Parse.zig");

pub const TokenList = struct {
    idx: usize,
    tokens: [] Token,


    pub fn current(self: TokenList) ?Token {
        if (self.idx >= self.tokens.len) return null;
        return self.tokens[self.idx];
    }

    pub fn consume(self: *TokenList) void {
        self.idx += 1;
    }
    pub fn retreat(self: *TokenList) void {
        self.idx -= 1;
    }

    pub fn peek(self: TokenList) ?Token {
        if (self.idx + 1 >= self.tokens.len) return null;
        return self.tokens[self.idx + 1];
    }

    pub fn fromParseFile(p: *Parse) !TokenList {
        var list: std.ArrayList(Token) = .empty;
        errdefer list.deinit(p.gpa);
        var prevToken: ?Token = null;
        var stream = p.sourceFile;
        while (stream.len != 0) {
            const token = try readToken(&stream, p, prevToken);
            try list.append(p.gpa, token);
            prevToken = token;
        }
        return .{
            .idx = 0,
            .tokens = try list.toOwnedSlice(p.gpa),
        };
    }

    pub fn deinit(self: *TokenList, alloc: std.mem.Allocator) void {
        alloc.free(self.tokens);
    }

    pub fn save(p: TokenList) usize {
        return p.idx;
    }

    pub fn load(p: *TokenList, idx: usize) void {
        p.idx = idx;
    }
};

pub const Token = struct {
    backingstr: [] const u8,
    skew: Skew,
    tag: Tag,

    pub const Skew = enum (i2) {
        left = -1,
        center = 0,
        right = 1,
    };

    /// Underscore prefixed items like `_number` indicate that the tag is nonliteral, while everything else means that set of symbols directly _ is itself its own symbol
    pub const Tag = enum {
        _invalid,

        _number,            //0d123, 0b010, 0xFF, 0o77, maybe one day z"1v32523945v76b23"
        _string,            //"abc"
        _multiline_string,
        _identifier,        //_abc, abc, Abc, Abcd3
        _asm_literal,       //@Func()
        _directive,         //$Ident

        @"+",
        @"-",
        @"+%",
        @"-%",
        @"+|",
        @"-|",
        @"*",
        @"*%",
        @"*|",
        @"/",
        @"%",
        
        @"+=",
        @"-=",
        @"+:=",
        @"-:=",
        @"+%=",
        @"-%=",
        @"+|=",
        @"-|=",
        @"*=",
        @"*:=",
        @"*%=",
        @"*|=",
        @"/=",
        @"%=",
        
        @"<<",
        @"<<%",
        @"<<|",
        @"<<<",
        @">>",
        @">>>",
        @"&",
        @"&~",
        @"^",
        @"^~",
        @"|",
        @"|~",

        @"<<=",
        @"<<:=",
        @"<<%=",
        @"<<|=",
        @"<<<=",
        @">>=",
        @">>>=",
        @"&=",
        @"&~=",
        @"^=",
        @"^~=",
        @"|=",
        @"|~=",
        
        @"~",
        @"!",

        @"==",
        @"!=",
        @"<",
        @"<=",
        @">=",
        @">",
        
        @"and",
        @"or",

        @"=",
        @":=",
        
        @"&mut",

        @"try",
        @"catch",
        @"orelse",
        
        @"fn",
        @"struct",
        @"enum",
        @"union",

        @"with",
        @"if",
        @"else",
        @"do",
        @"while",
        @"then",
        @"for",
        @"switch",

        @"break",
        @"continue",
        @"yield",
        @"defer",
        @"return",
        @"assume",
        @"assert",
        @"error",
        
        @"bitAlign",
        @"slice",
        @"array",
        @"int",
        _uintN,
        _sintN,
        _floatN,
        @"type",
        @"any",
        @"bool",
        @"void",

        @"infer",

        @"inline",

        @"const",
        @"var",
        @"volatile",
        @"view",
        @"mut",

        @"//",  // The // character for comments
        @"#",
        @"(",
        @")",
        @"_",
        @"{",
        @"}",
        @"[",
        @"]",
        @":",
        @";",
        @",",
        @".",
        @"?",
        @"=>",
        @"..",
        @"+..",

        //These just exist so my very simple statemachine tokenizer is happy, sadly if we emit these we just note that theyre illegal.
        @"+:",
        @"-:",
        @"*:",
        @"<<:",
        @"+.",
        @"&m",
        @"&mu",
        @"\\",
    };
};

const State = enum {
    start, 
    expect_ident_char,              //_a-zA-Z0-9
    expect_asm_literal_ident_char,   //_a-zA-Z0-9
    expect_directive_ident_char,   //_a-zA-Z0-9
    expect_base_or_base_10_digit,
    expect_base_10,
    expect_base_16,
    expect_base_2,
    expect_base_8,
    expect_base_85_or_end,
    expect_uint_bits_or_ident,
    expect_sint_bits_or_ident,
    expect_float_bits_or_ident,
    expect_symbol,
    expect_char_content,
    expect_char_escaped_char,
    expect_string_content,
    expect_string_escaped_char,
    expect_two_backslash,
    expect_backslash,
    expect_multiline_string_content,
    expect_quote_or_ident,
};

fn peekCharacter(stream: * [] const u8, length: usize) u8 {
    if (length - 1 >= stream.*.len) return '\n';
    return stream.*[length - 1];
}

fn remapInvalidSymbols(tag: Token.Tag) Token.Tag {
    return switch (tag) {
        .@"+:",
        .@"-:",
        .@"*:",
        .@"<<:",
        .@"+.",
        .@"&m",
        .@"&mu",
        .@"\\", => ._invalid,
        else => tag,
    };
}

/// Offsets the stream to the end of the token
fn _readToken(stream: * [] const u8) Token {
    var prefixWhitespace = false;
    if (std.ascii.isWhitespace(stream.*[0])) {
        prefixWhitespace = true;
        while (std.ascii.isWhitespace(stream.*[0])) {
            stream.* = stream.*[1..];
        }
    }
    var len: usize = 1;
    var suffixWhitespace = false;
    const tag: Token.Tag = sw: switch (State.start) {
        .start => switch (peekCharacter(stream, len)) {
            ' ', '\t', '\n', => unreachable,
            '"' => {
                len += 1;
                continue :sw .expect_string_content;
            },
            '\\' => {
                continue :sw .expect_two_backslash;
            },
            '\'' => {
                len += 1;
                continue :sw .expect_char_content;
            },
            '0' => {
                len += 1;
                continue :sw .expect_base_or_base_10_digit;
            },
            '1'...'9' => {
                len += 1;
                continue :sw .expect_base_10;
            },
            'u' => {
                len += 1;
                continue :sw .expect_uint_bits_or_ident;
            },
            'i' => {
                len += 1;
                continue :sw .expect_sint_bits_or_ident;
            },
            'f' => {
                len += 1;
                continue :sw .expect_float_bits_or_ident;
            },
            '@' => {
                len += 1;
                continue :sw .expect_asm_literal_ident_char;
            },
            '$' => {
                len += 1;
                continue :sw .expect_directive_ident_char;
            },
            '_', 'a'...'e', 'g', 'h', 'j'...'t', 'v'...'y', 'A'...'Z' => {
                len += 1;
                continue :sw .expect_ident_char;
            },
            'z' => {
                len += 1;
                continue :sw .expect_quote_or_ident;
            },
            else => continue :sw .expect_symbol,
        },
        .expect_ident_char => switch (peekCharacter(stream, len)) {
            '_', 'a'...'z', 'A'...'Z', '0'...'9' => {
                len += 1;
                continue :sw .expect_ident_char;
            },
            else => |char| {
                len -= 1;
                suffixWhitespace = std.ascii.isWhitespace(char);
                const backer = stream.*[0..len];
                if (backer[0] == '_' and backer.len > 1) break :sw ._identifier; //Anything starting with _ is an identifier
                break :sw std.meta.stringToEnum(Token.Tag, backer) orelse ._identifier;
                //if the "identifier" string is a known keyword, return it as that otherwise its an identifier
            },
        },
        .expect_sint_bits_or_ident, 
        .expect_uint_bits_or_ident, 
        .expect_float_bits_or_ident => |current_state| switch (peekCharacter(stream, len)) {
            '0'...'9' => {
                len += 1;
                continue :sw current_state;
            },
            '_', 'a'...'z', 'A'...'Z' => {
                len += 1;
                continue :sw .expect_ident_char;
            },
            else => |char| {
                len -= 1;
                suffixWhitespace = std.ascii.isWhitespace(char);
                break :sw switch (current_state) {
                    .expect_sint_bits_or_ident => ._sintN,
                    .expect_uint_bits_or_ident => ._uintN,
                    .expect_float_bits_or_ident => ._floatN,
                    else => unreachable,
                };
            },
        },
        .expect_asm_literal_ident_char, 
        .expect_directive_ident_char => |current_state| switch (peekCharacter(stream, len)) {
            '_', 'a'...'z', 'A'...'Z', '0'...'9' => {
                len += 1;
                continue :sw current_state;
            },
            else => |char| {
                len -= 1;
                suffixWhitespace = std.ascii.isWhitespace(char);
                break :sw switch (current_state) {
                    .expect_asm_literal_ident_char => ._asm_literal,
                    .expect_directive_ident_char => ._directive,
                    else => unreachable,
                };
            },
        },
        .expect_symbol => {
            if (len <= stream.*.len) b: {
                const backer = stream.*[0..len];
                if (std.meta.stringToEnum(Token.Tag, backer) == null) break :b;
                len += 1;
                continue :sw .expect_symbol;
            }
            len -= 1;
            break :sw std.meta.stringToEnum(Token.Tag, stream.*[0..len]) orelse std.debug.panic("Uhh, idk how but `{s}` isnt a symbol\n", .{stream.*[0..len]});
        },
        .expect_string_content => switch (peekCharacter(stream, len)){
            '"' => {
                break :sw ._string;
            },
            '\\' => {
                len += 1;
                continue :sw .expect_string_escaped_char;
            },
            else => {
                if (len >= stream.*.len) {
                    len -= 1;
                    break :sw ._invalid;
                }
                len += 1;
                continue :sw .expect_string_content;
            },
        },
        .expect_string_escaped_char => {
            len += 1;
            continue :sw .expect_string_content;
        },
        .expect_char_content => {
            switch (peekCharacter(stream, len)){
            '\'' => {
                break :sw ._number;
            },
            '\\' => {
                len += 1;
                continue :sw .expect_char_escaped_char;
            },
            '\n' => {
                len -= 1;
                break :sw ._invalid;
            },
            else => {
                if (len >= stream.*.len) {
                    len -= 1;
                    break :sw ._invalid;
                }
                len += 1;
                continue :sw .expect_char_content;
            },
        }},
        .expect_char_escaped_char => {
            len += 1;
            continue :sw .expect_char_content;
        },
        .expect_two_backslash => switch (peekCharacter(stream, len)) {
            ' ', '\t' => {
                len += 1;
                continue :sw .expect_two_backslash;
            },
            '\\' => {
                len += 1;
                continue :sw .expect_backslash;
            },
            else => |char| {
                len -= 1;
                suffixWhitespace = std.ascii.isWhitespace(char);
                break :sw ._multiline_string;
            },
        },
        .expect_backslash => switch (peekCharacter(stream, len)) {
            '\\' => {
                len += 1;
                continue :sw .expect_multiline_string_content;
            },
            else => {
                len -= 1;
                break :sw ._invalid;
            },
        },
        .expect_multiline_string_content => switch (peekCharacter(stream, len)) {
            '\n' => {
                if (len >= stream.*.len) {
                    len -= 1;
                    break :sw ._multiline_string;
                }
                len += 1;
                continue :sw .expect_two_backslash;
            },
            else => {
                len += 1;
                continue :sw .expect_multiline_string_content;
            }
        },
        .expect_base_or_base_10_digit => switch (peekCharacter(stream, len)) {
            'd' => {
                len += 1;
                continue :sw .expect_base_10;
            },
            'x' => {
                len += 1;
                continue :sw .expect_base_16;
            },
            'b' => {
                len += 1;
                continue :sw .expect_base_2;
            },
            'o' => {
                len += 1;
                continue :sw .expect_base_8;
            },
            '0'...'9' => {
                len += 1;
                continue :sw .expect_base_10;
            },
            else => |char| {
                len -= 1;
                suffixWhitespace = std.ascii.isWhitespace(char);
                break :sw ._number;
            },
        },
        .expect_base_2 => switch (peekCharacter(stream, len)) {
            '_', '0', '1' => {
                len += 1;
                continue :sw .expect_base_2;
            },
            else => |char| {
                len -= 1;
                suffixWhitespace = std.ascii.isWhitespace(char);
                break :sw ._number;
            },
        },
        .expect_base_8 => switch (peekCharacter(stream, len)) {
            '_', '0'...'7' => {
                len += 1;
                continue :sw .expect_base_8;
            },
            else => |char| {
                len -= 1;
                suffixWhitespace = std.ascii.isWhitespace(char);
                break :sw ._number;
            },
        },
        .expect_base_10 => switch (peekCharacter(stream, len)) {
            '_', '0'...'9' => {
                len += 1;
                continue :sw .expect_base_10;
            },
            else => |char| {
                len -= 1;
                suffixWhitespace = std.ascii.isWhitespace(char);
                break :sw ._number;
            },
        },
        .expect_base_16 => switch (peekCharacter(stream, len)) {
            '_', '0'...'9', 'a'...'f', 'A'...'F' => {
                len += 1;
                continue :sw .expect_base_16;
            },
            else => |char| {
                len -= 1;
                suffixWhitespace = std.ascii.isWhitespace(char);
                break :sw ._number;
            },
        },
        .expect_quote_or_ident => switch (peekCharacter(stream, len)) {
            '"' => {
                len += 1;
                continue :sw .expect_base_85_or_end;
            },
            else => continue :sw .expect_ident_char,
        },
        .expect_base_85_or_end => switch (peekCharacter(stream, len)) {
            '"' => {
                suffixWhitespace = std.ascii.isWhitespace(peekCharacter(stream, len+1));
                break :sw ._number;
            },
            '0'...'9', 'a'...'z', 'A'...'Z', '.', '-', ':', '+', '=', '^', '!', '/', 
            '*', '?', '&', '<', '>', '(', ')', '[', ']', '{', '}', '@', '%', '$', 
            '#' => {
                len += 1;
                continue :sw .expect_base_85_or_end;
            },
            else => {
                len -= 1;
                break :sw ._invalid;
            }
        },
        //inline else => |case| std.debug.panic("Unhandled case {} while parsing `{s}` in line `{s}`\n", .{case, stream.*[0..len-1], stream.*}),
    };
    defer stream.* = stream.*[len..];
    return .{
        .backingstr = stream.*[0..len],
        .skew = if (suffixWhitespace) (if (prefixWhitespace) Token.Skew.center else Token.Skew.left) else (if (prefixWhitespace) Token.Skew.right else Token.Skew.center),
        .tag = remapInvalidSymbols(tag),
    };
}

pub fn readToken(stream: * [] const u8, p: *Parse, prevToken: ?Token) !Token {
    const currToken = _readToken(stream);
    if (currToken.tag == ._invalid) {
        try p.newAnnot(.error_, "Invalid Token");
        try p.underlineSegment(currToken.backingstr, .{.mark = '^'});
        try p.throwAnnot();
    }
    if (prevToken) |prevTok| {
        if ((currToken.tag == ._number or currToken.tag == ._identifier) and prevTok.tag == ._number) {
            try p.newAnnot(.error_, "Invalid Digit Sequence");
            try p.underlineSegment(currToken.backingstr, .{.mark = '^'});
            try p.underlineSegment(prevTok.backingstr, .{.mark = '-'});
            try p.throwAnnot();
        }
        
    }
    

    return currToken;
}

test "Tokenization" {
    const expectEqual = std.testing.expectEqual;
    var msg: [] const u8 = undefined;
    var tkn: Token = undefined;

    msg = "if";
    tkn = _readToken(&msg);
    try expectEqual(Token.Tag.@"if", tkn.tag);

    msg = "if_";
    tkn = _readToken(&msg);
    try expectEqual(Token.Tag._identifier, tkn.tag);
    
    msg = "_if";
    tkn = _readToken(&msg);
    try expectEqual(Token.Tag._identifier, tkn.tag);
    
    msg = "_";
    tkn = _readToken(&msg);
    try expectEqual(Token.Tag.@"_", tkn.tag);

    msg = "$TypeOf";
    tkn = _readToken(&msg);
    try expectEqual(Token.Tag._directive, tkn.tag);

    msg = "@bsl";
    tkn = _readToken(&msg);
    try expectEqual(Token.Tag._asm_literal, tkn.tag);

    msg = "+:=";
    tkn = _readToken(&msg);
    try expectEqual(Token.Tag.@"+:=", tkn.tag);

    inline for (comptime std.meta.fieldNames(Token.Tag)) |name| {
        if (name[0] == '_') continue;
        msg = std.mem.sliceTo(name, 0);
        tkn = _readToken(&msg);
        try expectEqual(remapInvalidSymbols(comptime @field(Token.Tag, name)), tkn.tag);
    }

    msg = "u32";
    tkn = _readToken(&msg);
    try expectEqual(Token.Tag._uintN, tkn.tag);

    msg = "u3_2";
    tkn = _readToken(&msg);
    try expectEqual(Token.Tag._identifier, tkn.tag);

    msg = "u0";
    tkn = _readToken(&msg);
    try expectEqual(Token.Tag._uintN, tkn.tag);

    msg = "u00";
    tkn = _readToken(&msg);
    try expectEqual(Token.Tag._uintN, tkn.tag);

    msg = "u0xf";
    tkn = _readToken(&msg);
    try expectEqual(Token.Tag._identifier, tkn.tag);

    msg = "i32";
    tkn = _readToken(&msg);
    try expectEqual(Token.Tag._sintN, tkn.tag);

    msg = "f32";
    tkn = _readToken(&msg);
    try expectEqual(Token.Tag._floatN, tkn.tag);

    msg = 
    \\"abc"
    ;
    tkn = _readToken(&msg);
    try expectEqual(Token.Tag._string, tkn.tag);

    msg = 
    \\"abc\\n"
    ;
    tkn = _readToken(&msg);
    try expectEqual(Token.Tag._string, tkn.tag);

    msg = 
    \\" \\" "
    ;
    tkn = _readToken(&msg);
    try expectEqual(Token.Tag._string, tkn.tag);

    msg = "\\\\abc";
    tkn = _readToken(&msg);
    try expectEqual(Token.Tag._multiline_string, tkn.tag);

    msg = "\\\\abc\n  \\\\  \\def\n";
    tkn = _readToken(&msg);
    try expectEqual(Token.Tag._multiline_string, tkn.tag);

    msg = "'a'";
    tkn = _readToken(&msg);
    try expectEqual(Token.Tag._number, tkn.tag);

    msg = "\"hello oh no it doesnt end :(";
    tkn = _readToken(&msg);
    try expectEqual(Token.Tag._invalid, tkn.tag);

    msg = "'\\n'";
    tkn = _readToken(&msg);
    try expectEqual(Token.Tag._number, tkn.tag);

    msg = "'\\''";
    tkn = _readToken(&msg);
    try expectEqual(Token.Tag._number, tkn.tag);

    msg = "'\\x00'";
    tkn = _readToken(&msg);
    try expectEqual(Token.Tag._number, tkn.tag);

    msg = "1234567890";
    tkn = _readToken(&msg);
    try expectEqual(Token.Tag._number, tkn.tag);

    msg = "1_234_567_890";
    tkn = _readToken(&msg);
    try expectEqual(Token.Tag._number, tkn.tag);

    msg = "0d1234567890";
    tkn = _readToken(&msg);
    try expectEqual(Token.Tag._number, tkn.tag);

    msg = "0123456789";
    tkn = _readToken(&msg);
    try expectEqual(Token.Tag._number, tkn.tag);

    msg = "000";
    tkn = _readToken(&msg);
    try expectEqual(Token.Tag._number, tkn.tag);
    try expectEqual(3, tkn.backingstr.len);

    msg = "0b01";
    tkn = _readToken(&msg);
    try expectEqual(Token.Tag._number, tkn.tag);
    try expectEqual(4, tkn.backingstr.len);

    msg = "0b02";
    tkn = _readToken(&msg);
    try expectEqual(Token.Tag._number, tkn.tag);
    try expectEqual(3, tkn.backingstr.len);

    msg = "0x01";
    tkn = _readToken(&msg);
    try expectEqual(Token.Tag._number, tkn.tag);
    try expectEqual(4, tkn.backingstr.len);

    msg = "0o01";
    tkn = _readToken(&msg);
    try expectEqual(Token.Tag._number, tkn.tag);
    try expectEqual(4, tkn.backingstr.len);

    msg = "z\"2h1bc97b26vbb8B!*B&$^!@&B$B^(!69b17b26124\"";
    tkn = _readToken(&msg);
    try expectEqual(Token.Tag._number, tkn.tag);

    //Z85 encoding is allowed via z"" syntax where the stuff in the quotes is turned into a number
    msg = "z\"2h1bc97b26vbb8B!*\nB&$^!@&B$B^(!69b17b26124\"";
    tkn = _readToken(&msg);
    try expectEqual(Token.Tag._invalid, tkn.tag);
}

test "Test Token Errors" {
    var allocWriter: std.io.Writer.Allocating = .init(std.testing.allocator);
    defer allocWriter.deinit();
    var p: Parse = .{
        .gpa = std.testing.allocator,
        .writer = &allocWriter.writer,
        .sourceFile = 
            \\+:
        ,
        .qerror = null,
    };
    {
        try p.initError();
        defer p.printDeinitError();

        var file = p.sourceFile;
        _  = readToken(&file, &p, null) catch {};
    }

    {
        try p.initError();
        defer p.printDeinitError();

        p.sourceFile = 
            \\0b010102;
        ;
        var file = p.sourceFile;
        const first  = try readToken(&file, &p, null);
        _ = readToken(&file, &p, first) catch {};
    }

    {
        try p.initError();
        defer p.printDeinitError();

        p.sourceFile = 
            \\00FF1E;
        ;
        var file = p.sourceFile;
        const first  = try readToken(&file, &p, null);
        _ = readToken(&file, &p, first) catch {};
    }
    try std.testing.expectEqualStrings(
        \\  |
        \\0 |    +:
        \\  |    ^^
        \\ERROR: Invalid Token
        \\
        \\  |
        \\0 |    0b010102;
        \\  |    -------^
        \\ERROR: Invalid Digit Sequence
        \\
        \\  |
        \\0 |    00FF1E;
        \\  |    --^^^^
        \\ERROR: Invalid Digit Sequence
        \\
        \\
        , allocWriter.written()
    );
}

test "Test TokenList Errors" {
    var allocWriter: std.io.Writer.Allocating = .init(std.testing.allocator);
    defer allocWriter.deinit();
    var p: Parse = .{
        .gpa = std.testing.allocator,
        .writer = &allocWriter.writer,
        .sourceFile = 
            \\pub fn Main() !void {
            \\  return 0b0123456789;
            \\}
        ,
        .qerror = null,
    };
    {
        try p.initError();
        defer p.printDeinitError();

        const err = TokenList.fromParseFile(&p);
        try std.testing.expectError(error.compilation_failure, err);
    }
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