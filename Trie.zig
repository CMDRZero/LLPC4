const std = @import("std");
const Vec = std.ArrayList;

pub const Trie = struct {
    const Self = @This();
    pub const ID = enum (u32) {none = 0, _};

    alloc: std.mem.Allocator,
    maxID: ID = @enumFromInt(1),
    root: TrieNode = .{.next = .{.branch = ([_]?*TrieNode{null})**256}},
    table: Vec([] const u8),

    pub fn init(alloc: std.mem.Allocator) Trie {
        return Trie{
            .alloc = alloc,
            .table = .empty,
        };
    }

    pub fn deinit(self: *Trie, gpa: std.mem.Allocator) void {
        self.table.deinit(gpa);
        self.root.recuDeinit(gpa);
    }

    pub fn register(self: *Self, string: []const u8) !ID {
        var curr: *TrieNode = &self.root;
        for (string) |char| {
            switch (curr.next) {
                .branch => |*children| {
                    var child = children[char];
                    if (child == null) {
                        child = try self.alloc.create(TrieNode);
                        children[char] = child;
                        child.?.next = .{.branch = ([_]?*TrieNode{null})**256};
                    }
                    curr = child.?;
                }, .leaf => unreachable, 
            }
        }
        if (curr.next == .leaf) unreachable;
        if (curr.next.branch[0] != null) return error.Duplicate;
        curr.next.branch[0] = try self.alloc.create(TrieNode);
        curr.next.branch[0].?.next = .{ .leaf = self.maxID };
        defer self.maxID = @enumFromInt(1 + @intFromEnum(self.maxID));
        try self.table.append(self.alloc, string);
        return self.maxID;
    }

    pub fn lookup(self: Self, string: [] const u8) ?ID {
        var curr: TrieNode = self.root;
        for (string) |char| {
            switch (curr.next) {
                .branch => |children| curr = (children[char] orelse return null).*, 
                .leaf => unreachable, 
            }
        }
        if (curr.next == .leaf) unreachable;
        curr = (curr.next.branch[0] orelse return null).*;
        if (curr.next == .branch) return null;
        return curr.next.leaf;
    }

    pub fn writeGet(self: *Self, string: []const u8) !ID {
        return self.lookup(string) orelse try self.register(string);
    }

    pub fn getString(self: Self, key: ID) ? [] const u8 {
        std.debug.assert(@intFromEnum(key) <= @intFromEnum(self.maxID));
        return self.table.items[@intFromEnum(key) - 1];
    }
};

const TrieNode = struct {
    next: TrieNext,

    fn recuDeinit(self: TrieNode, gpa: std.mem.Allocator) void {
        switch (self.next) {
            .branch => |b| {
                for (b) |qx| {
                    if (qx) |x| {
                        defer gpa.destroy(x);
                        x.recuDeinit(gpa);
                    }
                }
            }, .leaf => {}
        }
    }
};

const TrieNext = union (enum) {
    branch: [256]?*TrieNode,
    leaf: Trie.ID,
};

test "Register Names" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var tree: Trie = .init(alloc);
    const xid = try tree.register("x"[0..]);
    try std.testing.expect(xid == @as(Trie.ID, @enumFromInt(1)));

    var abc = "abc";
    const abcid = try tree.register(abc[0..]);
    try std.testing.expect(abcid == @as(Trie.ID, @enumFromInt(2)));

    try std.testing.expectError(error.Duplicate, tree.register(abc[0..]));

    try std.testing.expectEqual(@as(Trie.ID, @enumFromInt(2)), tree.lookup(abc[0..]));

    try std.testing.expect(@as(Trie.ID, @enumFromInt(3)) == try tree.register("a"[0..]));
}