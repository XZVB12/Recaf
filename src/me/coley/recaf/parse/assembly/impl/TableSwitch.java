package me.coley.recaf.parse.assembly.impl;

import me.coley.recaf.bytecode.insn.LabeledTableSwitchInsnNode;
import me.coley.recaf.parse.assembly.Assembler;
import me.coley.recaf.parse.assembly.util.GroupMatcher;
import org.objectweb.asm.tree.AbstractInsnNode;

import java.util.HashMap;
import java.util.function.Function;

/**
 * TableSwitch assembler
 * <pre>
 *     range[&lt;RANGE&gt;] offsets[&lt;OFFSET/LABEL&gt;...] dflt[&lt;OFFSET/LABEL&gt;]
 * Examples:
 *     range[0-2] offsets[A, B, C] default[D]
 *     range[0-2] off[A, B, C] dflt[D]
 *     [0-2] [A, B, C] [D]
 * </pre>
 * Section identifiers may be shortened as shown, or not present,
 * as long as the content of each section is valid.
 *
 * @author Matt
 */
public class TableSwitch extends Assembler {
	/**
	 * Matcher for the switch.
	 */
	private final static GroupMatcher matcher =
			new GroupMatcher("(range)?\\[({RANGE}\\d+-\\d+)\\](\\soffsets|\\soff|\\s)\\[" +
					"({OFFSETS}.+?)\\](\\sdflt|\\sdefault|\\s)\\[({DEFAULT}.+?)\\]",
					new HashMap<String, Function<String, Object>>() {{
						put("RANGE", (s -> s));
						put("OFFSETS", (s -> s));
						put("DEFAULT", (s -> s));
					}});

	public TableSwitch(int opcode) {super(opcode);}

	@Override
	public AbstractInsnNode parse(String text) {
		if(matcher.run(text)) {
			String range = matcher.get("RANGE");
			String offsets = matcher.get("OFFSETS");
			String dflt = matcher.get("DEFAULT");
			String[] rangeSplit = range.split("-");
			int min = Integer.parseInt(rangeSplit[0]);
			int max = Integer.parseInt(rangeSplit[1]);
			String[] offsetsSplit = offsets.split(",\\s?");
			if (offsetsSplit.length == 0)
				fail(text, "Failed to parse offsets");
			if ((max - min) != offsetsSplit.length - 1)
				fail(text, "Range difference size does not match number of given offsets");
			return new LabeledTableSwitchInsnNode(min, max, dflt, offsetsSplit);
		}
		return fail(text);
	}
}