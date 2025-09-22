const std = @import("std");
const Allocator = std.mem.Allocator;
const Parse = @import("Parse.zig");

const int = std.math.big.int;

const release_safety = switch(@import("builtin").mode) {
    .Debug, .ReleaseSafe => true,
    .ReleaseFast, .ReleaseSmall => false,
};

pub const AddrLock = struct {
    addr: if (std.debug.runtime_safety) ?*AddrLock else void,
    /// The initial, unlocked, state of an `AddrLock`.
    pub const init: AddrLock = .{ .addr = null };
    pub fn lock(al: *AddrLock) void {
        std.debug.assert(al.addr == null);
        al.addr = al;
    }
    pub fn unlock(al: *AddrLock) void {
        std.debug.assert(al == al.addr.?); // the `AddrLock` is currently locked and has not moved
        al.addr = null;
    }
    /// If `al` is locked, assert that it is at the same address as when it was locked.
    pub fn check(al: *AddrLock) void {
        if (al.addr) |expected_addr| {
            std.debug.assert(al == expected_addr); // the `AddrLock` has not moved
        }
    }
    pub fn locked(al: AddrLock) bool {
        return al.addr != null;
    }
    pub fn unlocked(al: AddrLock) bool {
        return al.addr == null;
    }
};

pub fn parseInt(alloc: Allocator, p: Parse, str: [] const u8) !int.Const {
    const ret = try int.Managed.initSet(alloc, 0); 
    var val = str;
    var base: usize = 10;
    if (str[0] == '0' and str[1] == 'd') {
        val = val[2..];
    } else if (str[0] == '0' and str[1] == 'x') {
        base = 16;
        val = val[2..];
    } else if (str[0] == '0' and str[1] == 'o') {
        base = 8;
        val = val[2..];
    } else if (str[0] == '0' and str[1] == 'b') {
        base = 2;
        val = val[2..];
    }
    const baseAsBigInt = try int.Managed.initSet(alloc, base);
    for (val) |c| {
        if (c == '_') continue;
        try ret.mul(&ret, &baseAsBigInt);
        const digitValue = switch ()
    }
}