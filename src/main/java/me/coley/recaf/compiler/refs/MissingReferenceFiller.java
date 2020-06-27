package me.coley.recaf.compiler.refs;

import me.coley.analysis.SimAnalyzer;
import me.coley.analysis.SimInterpreter;
import me.coley.analysis.util.FrameUtil;
import me.coley.analysis.value.AbstractValue;
import me.coley.recaf.control.gui.GuiController;
import me.coley.recaf.util.ClassUtil;
import me.coley.recaf.util.Log;
import me.coley.recaf.util.TypeUtil;
import me.coley.recaf.workspace.JavaResource;
import me.coley.recaf.workspace.Workspace;
import org.objectweb.asm.*;
import org.objectweb.asm.tree.*;
import org.objectweb.asm.tree.analysis.Frame;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

public class MissingReferenceFiller {
	// TODO: What about "sun/" and "com/sun/"?
	private static final String[] JAVA_PREFIXES = {
			"java/", "javax/"
	};
	private Map<String, OutlinedType> internalToType = new HashMap<>();
	
	public static void run(GuiController controller, String path) {
		ClassReader cr = controller.getWorkspace().getClassReader(path);
		new MissingReferenceFiller().doTheThing(controller, cr);
	}

	private void doTheThing(GuiController controller, ClassReader cr) {
		// Phase 1: Generate basic missing references
		cr.accept(new RefClassVisitor(), ClassReader.SKIP_FRAMES);
		// Phase 2: Use stack analysis to fill in hierarchy information
		ClassNode node = ClassUtil.getNode(cr, ClassReader.SKIP_FRAMES);
		for (MethodNode method : node.methods) {
			try {
				SimAnalyzer analyzer = new SimAnalyzer(new SimInterpreter());
				analyzer.setThrowUnresolvedAnalyzerErrors(false);
				doMoreThings(node, method, analyzer.analyze(node.name, method));
			} catch (Throwable t) {
				Log.warn("Analysis failed when attempting to generate missing references {}.{}{}",
						node.name, method.name, method.desc);
			}
		}
		// Phase 3: Remove types that exist in the workspace
		for (String type : new HashSet<>(internalToType.keySet())) {
			if (controller.getWorkspace().getClassNames().contains(type))
				internalToType.remove(type);
			for (String prefix : JAVA_PREFIXES)
				if (type.startsWith(prefix))
					internalToType.remove(type);
		}
		// Phase 4: Write all types to classpath directory
	}

	private void doMoreThings(ClassNode node, MethodNode method, Frame<AbstractValue>[] frames) {
		// Skip if no instructions
		if (method.instructions == null)
			return;
		// Iterate over instructions
		for (int i = 0; i < method.instructions.size(); i++) {
			AbstractInsnNode insn = method.instructions.get(i);
			if (insn.getType() == AbstractInsnNode.FIELD_INSN) {
				Frame<AbstractValue> frame = frames[i];
				// If not-static: Compare stack against field owner
				// If put-op: Compare stack against field type
				FieldInsnNode fin = (FieldInsnNode) insn;
				if (!isStaticOp(fin.getOpcode())) {
					doTypeStuff(getType(fin.owner), getType(FrameUtil.getStackFromTop(frame, 1).getType().getInternalName()));
				}
				if (fin.getOpcode() == Opcodes.PUTFIELD || fin.getOpcode() == Opcodes.PUTSTATIC){
					doTypeStuff(getType(fin.desc), getType(FrameUtil.getTopStack(frame).getType().getInternalName()));
				}
			} else if (insn.getType() == AbstractInsnNode.METHOD_INSN) {
				Frame<AbstractValue> frame = frames[i];
				// If not-static: Compare stack against method owner
				// Compare stack args against descriptor arg types
				MethodInsnNode min = (MethodInsnNode) insn;
				Type methodType = Type.getMethodType(min.desc);
				for (int a = 0; a < methodType.getArgumentTypes().length; a++) {
					Type argType = methodType.getArgumentTypes()[a];
					doTypeStuff(getType(argType.getInternalName()), getType(FrameUtil.getStackFromTop(frame, a).getType().getInternalName()));
				}
				if (!isStaticOp(min.getOpcode())) {
					doTypeStuff(getType(min.owner), getType(FrameUtil.getStackFromTop(frame, methodType.getArgumentTypes().length).getType().getInternalName()));
				}
			} else if (insn.getType() == AbstractInsnNode.INVOKE_DYNAMIC_INSN) {
				Frame<AbstractValue> frame = frames[i];
				InvokeDynamicInsnNode indy = (InvokeDynamicInsnNode) insn;
				// Arg check
				Type methodType = Type.getMethodType(indy.desc);
				for (int a = 0; a < methodType.getArgumentTypes().length; a++) {
					Type argType = methodType.getArgumentTypes()[a];
					doTypeStuff(getType(argType.getInternalName()), getType(FrameUtil.getStackFromTop(frame, a).getType().getInternalName()));
				}
			}
		}
	}

	private void doTypeStuff(OutlinedType expectedType, OutlinedType actualType) {
		// If the types do not match, we must assume the expected type is a parent of the actual type
		// TODO: Hierarchy stuff
	}

	private OutlinedType getType(String key) {
		// Trim descriptors
		if (key.endsWith(";"))
			key = key.substring(1, key.length() - 1);
		// Get/create
		return internalToType.computeIfAbsent(key, OutlinedType::new);
	}

	private void visitMethod(int opcode, String owner, String name, String descriptor) {
		if (!TypeUtil.isPrimitiveDesc(descriptor))
			getType(owner).addMethod(name, descriptor, isStaticOp(opcode));
		visitMethodType(Type.getMethodType(descriptor));
	}

	private void visitMethodType(Type methodType) {
		if (!TypeUtil.isPrimitiveDesc(methodType.getReturnType().getDescriptor()))
			getType(methodType.getReturnType().getInternalName());
		for (Type methodArgType : methodType.getArgumentTypes())
			if (!TypeUtil.isPrimitiveDesc(methodArgType.getDescriptor()))
				getType(methodArgType.getInternalName());
	}

	private void visitField(int opcode, String owner, String name, String descriptor) {
		if (!TypeUtil.isPrimitiveDesc(descriptor))
			getType(owner).addField(name, descriptor, isStaticOp(opcode));
	}

	private void visitObject(Object value) {
		if (value instanceof Handle)
			visitHandle((Handle) value);
		else if (value instanceof Type)
			getType(((Type) value).getInternalName());
	}

	private void visitHandle(Handle handle) {
		OutlinedType type = getType(handle.getOwner());
		if (handle.getDesc().startsWith("(")) {
			type.addMethod(handle.getName(), handle.getDesc(), isStaticTag(handle.getTag()));
			visitMethodType(Type.getMethodType(handle.getDesc()));
		} else {
			type.addField(handle.getName(), handle.getDesc(), isStaticTag(handle.getTag()));
		}
	}

	private boolean isStaticOp(int op) {
		return op == Opcodes.INVOKESTATIC || op == Opcodes.GETSTATIC || op == Opcodes.PUTSTATIC;
	}

	private boolean isStaticTag(int tag) {
		return tag == Opcodes.H_INVOKESTATIC || tag == Opcodes.H_GETSTATIC || tag == Opcodes.H_PUTSTATIC;
	}

	private boolean isInvokeTag(int tag) {
		return tag == Opcodes.H_INVOKESTATIC || tag == Opcodes.H_INVOKEVIRTUAL ||
				tag == Opcodes.H_INVOKEINTERFACE || tag == Opcodes.H_INVOKESPECIAL;
	}

	class RefClassVisitor extends ClassVisitor {
		public RefClassVisitor() {
			super(Opcodes.ASM8);
		}

		@Override
		public void visit(int version, int access, String name, String signature, String superName, String[] interfaces) {
			OutlinedType type = getType(name);

			// force create types
			getType(superName);
			for (String it : interfaces) {
				getType(it);
			}
		}

		@Override
		public AnnotationVisitor visitAnnotation(String descriptor, boolean visible) {
			return new RefAnnotationVisitor(getType(descriptor));
		}

		@Override
		public AnnotationVisitor visitTypeAnnotation(int typeRef, TypePath typePath, String descriptor, boolean visible) {
			return new RefAnnotationVisitor(getType(descriptor));
		}

		@Override
		public FieldVisitor visitField(int access, String name, String descriptor, String signature, Object value) {
			visitObject(value);
			return new RefFieldVisitor();
		}

		@Override
		public MethodVisitor visitMethod(int access, String name, String descriptor, String signature, String[] exceptions) {
			return new RefMethodVisitor();
		}
	}

	private class RefAnnotationVisitor extends AnnotationVisitor {
		private final OutlinedType annoType;

		public RefAnnotationVisitor(OutlinedType annoType) {
			super(Opcodes.ASM8);
			this.annoType = annoType;
		}

		@Override
		public void visit(String name, Object value) {
			Type type = Type.getType(value.getClass());
			annoType.addAnnoValue(name, type);
			// TODO: Fill out OutlinedType
			//  - Is the value an enum constant?
			//    - Mark outline as enum
			//  - Is the value a class/type?
			//    - Create outline of type
		}

		@Override
		public void visitEnum(String name, String descriptor, String value) {
			annoType.addAnnoValue(name, Type.getType(descriptor));
		}

		@Override
		public AnnotationVisitor visitAnnotation(String name, String descriptor) {
			return new RefAnnotationVisitor(getType(descriptor));
		}
	}

	private class RefFieldVisitor extends FieldVisitor {
		public RefFieldVisitor() {
			super(Opcodes.ASM8);
		}

		@Override
		public AnnotationVisitor visitAnnotation(String descriptor, boolean visible) {
			return new RefAnnotationVisitor(getType(descriptor));
		}

		@Override
		public AnnotationVisitor visitTypeAnnotation(int typeRef, TypePath typePath, String descriptor, boolean visible) {
			return new RefAnnotationVisitor(getType(descriptor));
		}
	}

	private class RefMethodVisitor extends MethodVisitor {
		public RefMethodVisitor() {
			super(Opcodes.ASM8);
		}

		@Override
		public AnnotationVisitor visitAnnotation(String descriptor, boolean visible) {
			return new RefAnnotationVisitor(getType(descriptor));
		}

		@Override
		public AnnotationVisitor visitTypeAnnotation(int typeRef, TypePath typePath, String descriptor, boolean visible) {
			return new RefAnnotationVisitor(getType(descriptor));
		}

		@Override
		public AnnotationVisitor visitParameterAnnotation(int parameter, String descriptor, boolean visible) {
			return new RefAnnotationVisitor(getType(descriptor));
		}

		@Override
		public AnnotationVisitor visitInsnAnnotation(int typeRef, TypePath typePath, String descriptor, boolean visible) {
			return new RefAnnotationVisitor(getType(descriptor));
		}

		@Override
		public AnnotationVisitor visitTryCatchAnnotation(int typeRef, TypePath typePath, String descriptor, boolean visible) {
			return new RefAnnotationVisitor(getType(descriptor));
		}

		@Override
		public AnnotationVisitor visitLocalVariableAnnotation(int typeRef, TypePath typePath, Label[] start, Label[] end, int[] index, String descriptor, boolean visible) {
			return new RefAnnotationVisitor(getType(descriptor));
		}

		@Override
		public void visitTypeInsn(int opcode, String type) {
			getType(type);
		}

		@Override
		public void visitFieldInsn(int opcode, String owner, String name, String descriptor) {
			visitField(opcode, owner, name, descriptor);
		}

		@Override
		public void visitMethodInsn(int opcode, String owner, String name, String descriptor, boolean isInterface) {
			visitMethod(opcode, owner, name, descriptor);
		}

		@Override
		public void visitInvokeDynamicInsn(String name, String descriptor, Handle bootstrapMethodHandle, Object... bootstrapMethodArguments) {
			visitHandle(bootstrapMethodHandle);
			visitMethodType(Type.getMethodType(descriptor));
			for (Object arg : bootstrapMethodArguments)
				visitObject(arg);
		}

		@Override
		public void visitLdcInsn(Object value) {
			visitObject(value);
		}

		@Override
		public void visitMultiANewArrayInsn(String descriptor, int numDimensions) {
			getType(descriptor);
		}

		@Override
		public void visitTryCatchBlock(Label start, Label end, Label handler, String type) {
			getType(type).markException();
		}

		@Override
		public void visitLocalVariable(String name, String descriptor, String signature, Label start, Label end, int index) {
			getType(descriptor);
		}
	}
}
