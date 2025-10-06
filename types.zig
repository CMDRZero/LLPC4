const std = @import("std");
const util = @import("Util.zig");
const Sign = std.builtin.Signedness;
const Alloc = std.mem.Allocator;
const Parse = @import("Parse.zig");
const Sema = @import("Sema.zig");
const SymbolTable = @import("SymbolTable.zig").SymbolTable;

const maxbits = 1<<16;
const bitsInUsize = @typeInfo(usize).int.bits;

const int = std.math.big.int;

pub const Root = union (enum) {
    int: RangedInt,
    struct_ref: StructRef,


    pub const StructRef = struct {
        idx: Sema.Struct.Idx,
    };
    
    pub const RangedInt = struct {
        sign: Sign,     //If numBits is null, ignore this and treat as unsigned.
        numBits: ?usize,  //Nvm, guess we're doing usize here cuz why tf not
        lowerBoundInc: int.Managed,
        upperBoundInc: int.Managed,
        
        fn init(alloc: ?Alloc, bits: ?usize, sign: ?Sign, lowerBoundInc: ?int.Managed, upperBoundInc: ?int.Managed ) !RangedInt {
            if (bits) |numBits| {
                const lb = lowerBoundInc orelse (try int.Managed.initSet(alloc.?, 0));
                const ub = upperBoundInc orelse b: {
                    var val = try int.Managed.initSet(alloc.?, 1);
                    try val.shiftLeft(&val, numBits);
                    var one = try int.Managed.initSet(alloc.?, 1);
                    defer one.deinit();
                    try val.sub(&val, &one);
                    break :b val;
                };
                const trueSign = sign orelse std.debug.panic("INTERNAL ERROR: Expected a definite sign for known bitlength ({}), got null\n", .{bits.?});

                return .{
                    .sign = trueSign,
                    .numBits = numBits,
                    .lowerBoundInc = lb,
                    .upperBoundInc = ub,
                };
            } else {
                const lb = lowerBoundInc orelse std.debug.panic("INTERNAL ERROR: Expected a definite lower bound for unbound int, got null\n", .{});
                const ub = upperBoundInc orelse std.debug.panic("INTERNAL ERROR: Expected a definite upper bound for unbound int, got null\n", .{});
                return .{
                    .sign = sign orelse .unsigned,
                    .numBits = null,
                    .lowerBoundInc = lb,
                    .upperBoundInc = ub,
                };
            } 
        }

        fn deinit(self: *RangedInt) void {
            self.lowerBoundInc.deinit();
            self.upperBoundInc.deinit();
        }

        pub fn format(self: @This(), writer: anytype) !void {
            if (self.numBits) |bits| {
                if (self.sign == .unsigned) {
                    try writer.print("u{}[", .{bits});
                } else {
                    try writer.print("i{}[", .{bits});
                }
            } else {
                try writer.print("int[", .{});
            }
            try self.lowerBoundInc.formatNumber(writer, .{});
            try writer.print("..", .{});
            try self.upperBoundInc.formatNumber(writer, .{});
            try writer.print("]", .{});
            
        }

        fn canFitInto(self: RangedInt, other: RangedInt) bool {
            return self.lowerBoundInc.order(other.lowerBoundInc) != .lt and self.upperBoundInc.order(other.upperBoundInc) != .gt;
        }
    };


};

pub const AstReference = struct {
    prefix: Reference.Prefix,
    referent: SymbolTable.Reference,
};
pub const Reference = struct {
    pub const Prefix = union (enum) {
        pointer,
        pointerToMany,
        slice,
    };

    prefix: Prefix,
    referent: *Type,
};

pub const Base = union (enum) {
    root: Root,
    reference: Reference,
};

const AggregateTag = std.meta.Tag(Aggregate);
pub const AstAggregate = union (enum) {
    prefixed: Prefixed,

    orderedTuple: Aggregate.OrderedTuple,
    fieldNamedTuple: Aggregate.FieldNamedTuple,

    base: Base,

    pub const Prefixed = struct {
        prefix: Prefix,
        aggregate: SymbolTable.Reference,
    };

    pub const Prefix = union (enum) {
        array: SymbolTable.Reference,
        nullable,
        errorable,
    };
};
pub const Aggregate = union (enum) {
    prefixed: Prefixed,

    orderedTuple: OrderedTuple,
    fieldNamedTuple: FieldNamedTuple,

    base: Base,

    fn fromAst(p: *Parse, ref: SymbolTable.Reference) !Aggregate {
        const ast: AstAggregate = try p.fromRef(ref, AstAggregate);
        return switch (ast) {
            .prefixed => |pref| b: {
                const ptr = try p.arena.create(Aggregate);
                ptr.* = try .fromAst(p, pref.aggregate);
                break :b .{.prefixed = .{
                    .prefix = try .fromAst(p, pref.prefix),
                    .aggregate = ptr,
                }};
            },
            .orderedTuple => |tup| .{.orderedTuple = tup},
            .fieldNamedTuple => |tup| .{.fieldNamedTuple = tup},
            .base => |b| .{.base = b},
        };
    }

    pub const OrderedTuple = [] Type;
    pub const FieldNamedTuple = [] NameType;

    pub const Prefixed = struct {
        prefix: Prefix,
        aggregate: *Aggregate,
    };

    pub const Prefix = union (enum) {
        array: usize,
        nullable,
        errorable,

        fn fromAst(p: *Parse, ast: AstAggregate.Prefix) !Prefix {
            return switch (ast) {
                .array => |refN| .{.array = try p.fromRef(refN, usize)},
                .nullable => .{.nullable = {}},
                .errorable => .{.errorable = {}},
            };
        }
    };

    pub const NameType = struct {
        name: [] const u8,
        @"type": Type,
    };
};

pub const AstType = struct {
    data: Type.DataQualifier,
    access: Type.AccessQualifier,
    tweaks: Tweak,
    aggregate: SymbolTable.Reference,

    pub const Tweak = struct {
        alignment: ?SymbolTable.Reference,
    };  
};
pub const Type = struct {
    data: DataQualifier,
    access: AccessQualifier,
    tweaks: Tweak,
    aggregate: Aggregate,

    pub const Tweak = struct {
        alignment: usize,
    };  

    pub const AccessQualifier = enum {
        view,
        mut,
    };

    pub const DataQualifier = enum {
        @"const",
        @"var",
        @"volatile",
    };

    pub fn resolve(p: *Parse, ref: SymbolTable.Reference) !Type {
        return .fromAst(p, try p.fromRef(ref, AstType));
    }

    fn fromAst(p: *Parse, ast: AstType) !Type {
        var self: Type = .{
            .data = ast.data,
            .access = ast.access,
            .tweaks = .{.alignment = undefined},
            .aggregate = try .fromAst(p, ast.aggregate),
        };
        if (ast.tweaks.alignment) |refAlign| {
            self.tweaks.alignment = try p.fromRef(refAlign, usize);
        } else {
            self.tweaks.alignment = self.naturalAlignment();
        }
        
        return self;
    }

    fn naturalAlignment(self: Type) usize {
        _ = self;
        return 1;
    }

    pub const CoercionError = union (enum) {
        incompatible_data_qualifiers: struct {
            from: DataQualifier,
            to: DataQualifier,
        },
        discard_view: void,
        widens_alignment: struct {
            from: usize,
            to: usize,
        },
        incompatible_aggregates: struct {
            from: AggregateTag,
            to: AggregateTag,
        },
        incompatible_aggregate_prefixes: struct {
            from: Aggregate.Prefix,
            to: Aggregate.Prefix,
        },
        varient_field_counts: struct {
            from: usize,
            to: usize,
        },
        missing_field: struct {
            from: enum {
                orig,
                dest,
            },
            name: [] const u8,
        }
    };
    pub fn canCoerceTo(lhs: Type, rhs: Type) bool {
        return canCoerceToVerb(lhs, rhs) == null;
    }

    /// Either returns an error or null (a success) 
    pub fn canCoerceToVerb(lhs: Type, rhs: Type) ?CoercionError {
        if (&lhs == &rhs) {
            //Short circuit lol
            return null;
        }
        if (lhs.data != rhs.data) return .{
            .incompatible_data_qualifiers = .{
                .from = lhs.data, 
                .to = rhs.data,
        }};
        if (lhs.access == .view and rhs.access == .mut) return .{
            .discard_view = {},
        };
        // const lhsAlign = 
        // if (lhs.tweaks.alignment < rhs.tweaks.alignment) return .{
        //     .widens_alignment = .{
        //         .from = lhs.tweaks.alignment, 
        //         .to = rhs.tweaks.alignment,
        // }};
        
        const lagr: Aggregate = lhs.aggregate;
        const ragr: Aggregate = rhs.aggregate;

        return cctAggrAggr(lagr, ragr);
    }

    fn cctAggrAggr(lagr: Aggregate, ragr: Aggregate) ?CoercionError {
        var do = true;
        while (do) : (do = false) {
            //continue --> throw error
            //break --> skip
            //Noop --> break --> skip
            switch (lagr) {
                .prefixed => |lpref| switch (ragr) {
                    .prefixed => |rpref| if (cctPrefixPrefix(lpref.prefix, rpref.prefix)) |err| return err,
                    else => continue,
                },
                .orderedTuple => |ltup| switch (ragr) {
                    .orderedTuple => |rtup| if (cctOrderedTupleTuple(ltup, rtup)) |err| return err,
                    else => continue,
                },
                .fieldNamedTuple => |ltup| switch (ragr) {
                    .fieldNamedTuple => |rtup| if (cctNamedTupleTuple(ltup, rtup)) |err| return err,
                    else => continue,
                },
                .base => |lbase| switch (ragr) {
                    .base => |rbase| if (cctBaseBase(lbase, rbase)) |err| return err,
                    .prefixed => |rpref| if (cctBasePrefix(lbase, rpref)) |err| return err,
                    else => continue,
                },
            }
            break;
        } else {
            return .{
                .incompatible_aggregates = .{
                    .from = lagr,
                    .to = ragr,
            }};
        }

        return null;
    }

    fn cctPrefixPrefix(lhs: Aggregate.Prefix, rhs: Aggregate.Prefix) ?CoercionError {
        while (true) {
            switch (lhs) {
                .array => |N1| switch (rhs) {
                    .array => |N2| if (N1 == N2) return null,
                    else => break,
                },
                else => |tag1| switch (rhs) {
                    else => |tag2| if (std.meta.eql(tag1, tag2)) return null,
                }
            }
            break;
        }
        return .{
            .incompatible_aggregate_prefixes = .{
                .from = lhs,
                .to = rhs,
        }};
    }

    fn cctOrderedTupleTuple(lhs: [] Type, rhs: [] Type) ?CoercionError {
        if (lhs.len != rhs.len) return .{
            .varient_field_counts = .{
                .from = lhs.len,
                .to = rhs.len,
        }};
        for (lhs, rhs) |ltype, rtype| {
            if (ltype.canCoerceToVerb(rtype)) |err| return err;
        }
        return null;
    }

    fn cctNamedTupleTuple(lhs: [] Aggregate.NameType, rhs: [] Aggregate.NameType) ?CoercionError {
        if (lhs.len != rhs.len) return .{
            .varient_field_counts = .{
                .from = lhs.len,
                .to = rhs.len,
        }};
        for (lhs) |pair| {
            for (rhs) |other| {
                if (std.mem.indexOfDiff(u8, pair.name, other.name) == null) {
                    if (pair.@"type".canCoerceToVerb(other.@"type")) |err| return err;
                    break;
                }
            } else {
                return .{
                    .missing_field = .{
                        .from = .dest,
                        .name = pair.name,
                }};
            }
        }
        for (rhs) |pair| {
            for (lhs) |other| {
                if (std.mem.indexOfDiff(u8, pair.name, other.name) == null) break;
            } else {
                return .{
                    .missing_field = .{
                        .from = .orig,
                        .name = pair.name,
                }};
            }
        }
        return null;
    }
    
    fn cctBaseBase(lhs: Base, rhs: Base) ?CoercionError {
        _ = lhs;
        _ = rhs;
        return null;
    }

    fn cctBasePrefix(lhs: Base, rhs: Aggregate.Prefixed) ?CoercionError {
        switch (rhs.prefix) {
            .nullable, .errorable => if (cctAggrAggr(Aggregate{.base = lhs}, rhs.aggregate.*)) |err| return err,
            else => return .{
                .incompatible_aggregates = .{
                    .from = Aggregate{.base = lhs},
                    .to = .prefixed,
            }},
        }

        return null;
    }
};

fn freeInt(val: int.Const, alloc: Alloc) void {
    alloc.free(val.limbs);
}

test "int[l..b]" {
    const expectEqual = std.testing.expectEqualStrings;
    const testingAlloc = std.testing.allocator;
    
    var allocWriter: std.io.Writer.Allocating = .init(testingAlloc);
    defer allocWriter.deinit();
    var writer: *std.io.Writer = &allocWriter.writer;

    try writer.print("ABC = {s}", .{"ABC"});
    try expectEqual(
        "ABC = ABC",
        allocWriter.written(),
    );
    
    allocWriter.clearRetainingCapacity();
    var p: Parse = try .init(std.testing.allocator, &allocWriter.writer,
        \\int[0..100]
    , .{});
    defer p.deinit();

    const lbs = p.sourceFile[4..5];
    const ubs = p.sourceFile[7..10];
    var lb = try Parse.computeParsedInteger(p.gpa, lbs);
    errdefer lb.deinit();
    var ub = try Parse.computeParsedInteger(p.gpa, ubs);
    errdefer ub.deinit();
    
    var ri: Root.RangedInt = try .init(testingAlloc, null, null, lb, ub);
    defer ri.deinit();
    try writer.print("{f}", .{ri});
    try expectEqual(
        "int[0..100]",
        allocWriter.written(),
    );
}

test "uN[..]" {
    const expectEqual = std.testing.expectEqualStrings;
    const testingAlloc = std.testing.allocator;
    
    var allocWriter: std.io.Writer.Allocating = .init(testingAlloc);
    defer allocWriter.deinit();
    var writer: *std.io.Writer = &allocWriter.writer;

    var ri: Root.RangedInt = try .init(testingAlloc, 32, .unsigned, null, null);
    defer ri.deinit();
    try writer.print("{f}", .{ri});
    try expectEqual(
        "u32[0..4294967295]",
        allocWriter.written(),
    );
}

test "Coercion" {
    const file = 
        \\u32 uptr
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
        
        const orig: Type = try .resolve(&p, (try p.parseType()).?);
        const dest: Type = try .resolve(&p, (try p.parseType()).?);
        const result = orig.canCoerceToVerb(dest);
        try std.testing.expectEqual(null, result);
    }

    try std.testing.expectEqualStrings(
        ""
        , allocWriter.written()
    );
}