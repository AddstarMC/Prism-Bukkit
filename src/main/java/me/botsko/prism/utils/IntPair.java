package me.botsko.prism.utils;

public class IntPair {
	public IntPair() {
	}

	public IntPair(int first, int second) {
		this.first = first;
		this.second = second;
	}

	public IntPair(long both) {
		first = (int) ((both >> 32) & 0xFFFFFFFF);
		second = (int) ((both) & 0xFFFFFFFF);
	}

	public int first;
	public int second;

	public long getBoth() {
		return (((long) first) << 32) | second;
	}

	@Override
	public int hashCode() {
		return Long.hashCode(getBoth());
	}
}
