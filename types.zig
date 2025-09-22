const std = @import("std");
const util = @import("Util.zig");
const Sign = std.builtin.Signedness;
const Alloc = std.mem.Allocator;

const maxbits = 1<<16;
const bitsInUsize = @typeInfo(usize).int.bits;

const Int = std.math.big.int.Managed;

const Root = union (enum) {
    const RangedInt = struct {
        pin: util.AddrLock = .init,
        sign: Sign,     //If numBits is null, ignore this and treat as unsigned.
        numBits: ?usize,  //Nvm, guess we're doing usize here cuz why tf not
        lowerBoundInc: Int,
        upperBoundInc: Int,
        
        fn init(alloc: ?Alloc, bits: ?u16, sign: ?Sign, lowerBoundInc: ?[] const u8, upperBoundInc: ?[] const u8 ) RangedInt {
            if (sign == null and bits != null) std.debug.panic("INTERNAL ERROR: Expected a definite sign for known bitlength ({}), got null\n", .{bits.?});
            const lowerBase = 
            if (bits != null and bits.? <= bitsInUsize) {
                return .{
                    .pin = .init,
                    .sign = implSign,
                    .bits = bits,
                    .lowerBoundInc = .{.small = lowerBoundInc orelse 0},
                    .upperBoundInc = .{.small = upperBoundInc orelse (~@as(usize, 0) << (bitsInUsize - bits.?) >> (bitsInUsize - bits.?))},
                };
            } else {
                if (upperBoundInc == null) std.debug.panic("INTERNAL ERROR: Expected a definite upperbound for unknown bitlength, got null\n", .{});
                if (lowerBoundInc == null) std.debug.panic("INTERNAL ERROR: Expected a definite lowerbound for unknown bitlength, got null\n", .{});
                return .{
                    .pin = .init,
                    .sign = implSign,
                    .bits = bits,
                    .lowerBoundInc = try .initSet(alloc.?, lowerBoundInc.?),
                    .upperBoundInc = try .initSet(alloc.?, upperBoundInc.?),
                };
            }
        }

        pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
            if (self.numBits) |bits| {

            } else {

            }
            return writer.print("<{s}: {s}>", .{ value.name, value.ty });
        }
    };
};

test "Ranged Ints" {
    const expectEqual = std.testing.expectEqual;
    const testingAlloc = std.testing.allocator;


}