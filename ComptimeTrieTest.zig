const trie = @import("Trie.zig");
const std = @import("std");

test "Idk" {
    var x: usize = 0;
    const y: comptime_int = 2;
    var z: f64 = 1;


    const lookup: std.StaticStringMap(u32) = .initComptime(b: {
        comptime { 
            var strings: struct {[] const u8} = .{&[_]u8{}};
            for ([_]type{
                void, bool,
                u1, u8, u16, u32, u64, usize,
                f16, f32, f64, f128,
                comptime_int, comptime_float,
            }) |T| {
                strings = strings ++ .{@typeName(T)};
            }
            break :b strings;
        }
    });

    _ = &x;
    _ = &y;
    _ = &z;

    std.debug.print("Id is {}", .{comptime lookup.get(@typeName(@TypeOf(x)))});
}