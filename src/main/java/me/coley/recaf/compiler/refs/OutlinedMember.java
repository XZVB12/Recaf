package me.coley.recaf.compiler.refs;

public class OutlinedMember {
	private final String name;
	private final String desc;
	private boolean isStatic;

	public OutlinedMember(String name, String desc, boolean isStatic) {
		this.name = name;
		this.desc = desc;
		this.isStatic = isStatic;
	}

	public String getName() {
		return name;
	}

	public String getDesc() {
		return desc;
	}

	public boolean isStatic() {
		return isStatic;
	}
}
