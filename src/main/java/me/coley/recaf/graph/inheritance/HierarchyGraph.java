package me.coley.recaf.graph.inheritance;

import me.coley.recaf.graph.*;
import me.coley.recaf.workspace.Workspace;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.tree.ClassNode;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Graph model to represent the class inheritance of a loaded input. <br>
 * The graph is generative, meaning the graph's vertices and edges are dynamically generated when
 * requested. The relations are not stored due to their modifiable nature.
 *
 * @author Matt
 */
public class HierarchyGraph extends WorkspaceGraph<HierarchyVertex> {
	/**
	 * Map of parent to children names.
	 */
	private final Map<String, Set<String>> descendents = new HashMap<>();

	/**
	 * Constructs a hierarchy from the given workspace.
	 *
	 * @param workspace
	 * 		Workspace to pull classes from.
	 */
	public HierarchyGraph(Workspace workspace) {
		super(workspace);
		refresh();
	}


	@Override
	public HierarchyVertex getVertexFast(ClassReader key) {
		return new HierarchyVertex(this, key);
	}

	/**
	 * @param name
	 * 		Class name of a class belonging to some inheritance hierarchy.
	 *
	 * @return Inheritance hierarchy containing the given class.
	 */
	public Set<HierarchyVertex> getHierarchy(String name) {
		return getHierarchy(getVertex(name));
	}

	/**
	 * @param vertex
	 * 		Class vertex that belongs to some inheritance hierarchy.
	 *
	 * @return Inheritance hierarchy containing the given class.
	 */
	public Set<HierarchyVertex> getHierarchy(HierarchyVertex vertex) {
		if(vertex == null)
			return Collections.emptySet();
		ClassHierarchyBuilder builder = new ClassHierarchyBuilder();
		return builder.build(vertex).stream()
				.collect(Collectors.toSet());
	}

	/**
	 * @param name
	 * 		Class name.
	 *
	 * @return Direct descendants of the class.
	 */
	public Stream<String> getDescendants(String name) {
		if (descendents.containsKey(name))
			return descendents.get(name).stream();
		// Empty stream
		return Stream.of();
	}

	/**
	 * @param name
	 * 		Class name.
	 *
	 * @return All descendants of the class.
	 */
	public Stream<String> getAllDescendants(String name) {
		return (getDescendants(name).map(desc -> getAllDescendants(desc))
				.reduce(getDescendants(name), (master, children) -> Stream.concat(master, children)));
	}

	/**
	 * @param name
	 * 		Class name.
	 *
	 * @return Direct parents of the class.
	 */
	public Stream<String> getParents(String name) {
		HierarchyVertex vert = getVertex(name);
		if (vert != null)
			return getParents(vert);
		// Empty stream
		return Stream.of();
	}

	/**
	 * @param vertex
	 * 		Class vertex.
	 *
	 * @return Direct parents of the class.
	 */
	public Stream<String> getParents(HierarchyVertex vertex) {
		return Stream.concat(
				Stream.of(vertex.getData().getSuperName()),
				Stream.of(vertex.getData().getInterfaces()));
	}

	/**
	 * @param name
	 * 		Class name.
	 *
	 * @return All parents of the class.
	 */
	public Stream<String> getAllParents(String name) {
		return (getParents(name).map(desc -> getAllParents(desc))
				.reduce(getParents(name), (master, parents) -> Stream.concat(master, parents)));
	}

	/**
	 * Check if the given method in a class is linked to a locked library method.
	 *
	 * @param owner
	 * 		Class the method resides in.
	 * @param name
	 * 		Method name.
	 * @param desc
	 * 		Method descriptor.
	 *
	 * @return {@code true} if any class in the hierarchy of the owner is a library class and
	 * defines the given method,
	 */
	public boolean isLibrary(String owner, String name, String desc) {
		// Get classes that are considered "library" classes (not included in Input)
		Stream<HierarchyVertex> hierarchy = getHierarchy(owner).stream();
		Stream<HierarchyVertex> libClasses = hierarchy.filter(vertex -> !getWorkspace()
				.getPrimaryClassNames().contains(vertex.toString()));
		// Check if the library classes have a matching method.
		return libClasses
					.map(vertex -> vertex.getData())
					.map(reader -> {
						ClassNode node = new ClassNode();
						reader.accept(node, ClassReader.SKIP_DEBUG | ClassReader.SKIP_CODE);
						return node;
					}).anyMatch(node -> node.methods.stream()
							.anyMatch(method ->
								name.equals(method.name) && desc.equals(method.desc)));
	}

	/**
	 * Check if two methods are linked.
	 *
	 * @param owner1
	 * 		First method's defining class.
	 * @param name1
	 * 		First method's name.
	 * @param desc1
	 * 		First method's descriptor.
	 * @param owner2
	 * 		Second method's defining class.
	 * @param name2
	 * 		Second method's name.
	 * @param desc2
	 * 		Second method's descriptor.
	 *
	 * @return {@code true} if the two methods belong to the same hierarchy.
	 */
	public boolean areLinked(String owner1, String name1, String desc1, String owner2, String name2, String desc2) {
		// Obviously mis-matching definitions are not linked
		if (!name1.equals(name2) || !desc1.equals(desc2))
			return false;
		// Check if owner2 is in the same hierarchy as owner1.
		return getHierarchy(owner1).stream()
				.anyMatch(vertex -> owner2.equals(vertex.toString()));
	}

	// ============================== UTILITY =================================== //

	/**
	 * Populate {@link #descendents} map.
	 */
	public void refresh() {
		// TODO: Call this when the inheritance tree is modified.
		descendents.clear();
		for (ClassReader reader : getWorkspace().getPrimaryClassReaders()) {
			descendents.computeIfAbsent(reader.getSuperName(), k -> new HashSet<>()).add(reader.getClassName());
			for (String inter : reader.getInterfaces()) {
				descendents.computeIfAbsent(inter, k -> new HashSet<>()).add(reader.getClassName());
			}
		}
	}
}
