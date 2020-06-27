package me.coley.recaf.compiler.refs;

import org.objectweb.asm.Type;

import java.util.ArrayList;
import java.util.List;

public class OutlinedType {
	private final String name;
	private final List<String> interfaces = new ArrayList<>();
	private final List<OutlinedMember> fields = new ArrayList<>();
	private final List<OutlinedMember> methods = new ArrayList<>();
	private String superName;
	private boolean isEnum;
	private boolean isException;

	public OutlinedType(String name) {
		this.name = name;
	}

	public void markException() {
		isException = true;
		superName = "java/lang/Exception";
	}


	public void markEnum() {
		isEnum = true;
		superName = "java/lang/Enum";
		// add methods
		getMethods().add(new OutlinedMember("values", "()[L" + name + ";", true));
		getMethods().add(new OutlinedMember("valueOf", "(Ljava/lang/String;)L" + name + ";", true));
	}

	public void addAnnoValue(String name, Type type) {
		getMethods().add(new OutlinedMember(name, "()" + type.getDescriptor(), false));
	}

	public void addField(String name, String desc, boolean isStatic) {
		getFields().add(new OutlinedMember(name, desc, isStatic));
	}

	public void addMethod(String name, String desc, boolean isStatic) {
		getMethods().add(new OutlinedMember(name, desc, isStatic));
	}

	// =============================================================== //

	public String getName() {
		return name;
	}

	public String getSuperName() {
		return superName;
	}

	public void setSuperName(String superName) {
		this.superName = superName;
	}

	public List<String> getInterfaces() {
		return interfaces;
	}

	public List<OutlinedMember> getFields() {
		return fields;
	}

	public List<OutlinedMember> getMethods() {
		return methods;
	}
}
