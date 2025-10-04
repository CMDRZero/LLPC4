const std = @import("std");

fn newKey(ptr: *Trie) Trie.Key {
    return @enumFromInt(@intFromPtr(ptr));
}

pub const Trie = struct {
    pub const Key = enum (usize) {none = 0, _};
    data: BinaryStreamIntoString,
    next: [2]*Trie,
    key: Key,
    hasChildren: bool,

    pub const init: Trie = .{
        .data = .empty,
        .next = undefined,
        .key = @enumFromInt(1),
        .hasChildren = false,
    };

    pub fn addString(self: *Trie, gpa: std.mem.Allocator, string: [] const u8) !Key {
        return self.addStream(gpa, .fromString(string));
    }

    fn addStream(self: *Trie, gpa: std.mem.Allocator, stream: BinaryStreamIntoString) !Key {
        if (self.data.startsWith(stream)) |diff| {
            const origptr = try gpa.create(Trie);
            origptr.* = .{
                .data = self.data.spliceUpper(diff+1),
                .next = self.next,
                .key = self.key,
                .hasChildren = self.hasChildren,
            };
            const newptr = try gpa.create(Trie);
            newptr.* = .{
                .data = stream.spliceUpper(diff+1),
                .next = undefined,
                .key = newKey(origptr),
                .hasChildren = false,
            };
            const origBit = self.data.readBit(diff);
            self.next[origBit] = origptr;
            self.next[~origBit] = newptr;
            self.data = self.data.spliceLower(diff);
            self.key = newKey(newptr);
        } else if (stream.bitLength == self.data.bitLength) {
            return self.key;
        } else {
            const bit = stream.readBit(self.data.bitLength);
            return self.next[bit].addStream(gpa, stream.spliceUpper(self.data.bitLength + 1));
        }
    }
};

pub const BinaryStreamIntoString = struct {
    const Self = @This();
    const veclen = std.simd.suggestVectorLength(u1).?;
    
    ptr: [*] const u8,
    bitOffset: u3,
    bitLength: u29,

    pub const empty: Self = .{
        .ptr = undefined,
        .bitOffset = undefined,
        .bitLength = 0,
    };

    pub fn fromString(string: [] const u8) Self {
        return .{
            .ptr = string.ptr,
            .bitOffset = 0,
            .bitlength = string.len << 3,
        };
    }

    pub fn startsWith(self: Self, other: Self) ?u29 {
        if (other.len < self.len) return other.len;
        const len = self.len;
        
        for (0..len / veclen) |chunk| {
            const selfV = vectorizeChunk(self.ptr + chunk * veclen / 8, self.bitOffset);
            const otherV = vectorizeChunk(other.ptr + chunk * veclen / 8, other.bitOffset);
            const diff = std.simd.firstIndexOfValue(selfV ^ otherV, 1);
            if (diff) |d| return chunk * veclen + d;
        }
        for (0..len % veclen) |i_| {
            const i = (len / veclen) * veclen + i_;
            if (self.readBit(i) != other.readBit(i)) return i;
        }
        return null;
    }

    pub fn vectorizeChunk(ptr: [*] const u8, offset: u3) @Vector(veclen, u1) {
        const roundedlen = std.math.divCeil(veclen + offset, 8) catch unreachable;
        const bits: [roundedlen * 8] u1 = @bitCast(ptr[0..roundedlen]);
        return @bitCast(bits[offset..][0..veclen]);
    }

    pub fn readBit(self: Self, idx: usize) u1 {
        return self.ptr[idx >> 3] >> (idx % 8);
    }

    pub fn spliceUpper(self: Self, idx: usize) Self {
        return self.offsetBy(idx).scaledBy(self.bitLength - idx);
    }

    pub fn spliceLower(self: Self, idx: usize) Self {
        return self.scaledBy(idx);
    }

    fn offsetBy(self: Self, offset: usize) Self {
        return .{
            .ptr = self.ptr + (offset + self.bitOffset) >> 3,
            .bitOffset = (offset + self.bitOffset) % 8,
            .bitLength = self.bitLength,
        };
    }

    fn scaledBy(self: Self, newLength: usize) Self {
        return .{
            .ptr = self.ptr,
            .bitOffset = self.bitOffset,
            .bitLength = newLength,
        };
    }  
};

test "Trie Test" {
    var t:
}