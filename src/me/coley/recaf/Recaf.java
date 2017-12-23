package me.coley.recaf;

import java.io.File;
import java.io.IOException;

import org.objectweb.asm.tree.ClassNode;

import me.coley.logging.Level;
import me.coley.recaf.asm.JarData;
import me.coley.recaf.config.Configs;
import me.coley.recaf.event.Bus;
import me.coley.recaf.event.impl.EClassSelect;
import me.coley.recaf.event.impl.EInit;
import me.coley.recaf.plugin.Plugins;
import me.coley.recaf.ui.SwingUI;
import me.coley.recaf.ui.component.panel.ClassDisplayPanel;
import me.coley.recaf.util.Swing;
import picocli.CommandLine;

public class Recaf {
	/**
	 * Singleton instance of recaf.
	 */
	public final static Recaf INSTANCE = new Recaf();
	/**
	 * Logging.
	 */
	public final Logging logging = new Logging();
	/**
	 * Event bus.
	 */
	public final Bus bus = new Bus();
	/**
	 * Plugin system.
	 */
	public final Plugins plugins = new Plugins();
	/**
	 * User interface.
	 */
	public final SwingUI ui = new SwingUI();
	/**
	 * Wrapper for multiple configurations.
	 */
	public final Configs configs = new Configs();
	/**
	 * Content of the current jar file.
	 */
	public JarData jarData;

	/**
	 * Setup things.
	 * 
	 * @param params
	 *            Optional command line arguements.
	 */
	private void setup(LaunchParams params) {
		logging.info("Setting up Recaf");
		logging.info("Loading config", 1);
		configs.init();
		logging.info("Creating UI", 1);
		configs.ui.setLookAndFeel(configs.ui.getLookAndFeel());
		ui.init(params);
		ui.setVisible();
		logging.info("Loading plugins", 1);
		logging.setLevelConsole(Level.values()[params.logConsole]);
		logging.setLevelFile(Level.values()[params.logFile]);
		plugins.init();
		bus.post(new EInit());
		logging.info("Finished setup");
		if (params.initialFile != null) {
			logging.info("Opening initial file: " + params.initialFile.getName());
			selectJar(params.initialFile);
			if (params.initialClass != null) {
				logging.info("Opening initial class: " + params.initialClass);
				selectClass(params.initialClass);
			}
		}
		Swing.fixLaunchLAF();
	}

	/**
	 * Sets the {@link #jarData current loaded jar}.
	 * 
	 * @param inJar
	 *            Jar file to read classes from.
	 */
	public void selectJar(File inJar) {
		try {
			jarData = new JarData(inJar);
			ui.refreshTree();
			ui.frame.setTitle("Recaf: " + inJar.getName());
		} catch (IOException e) {
			logging.error(e);
		}
	}

	/**
	 * Saves the current edits to the given file.
	 * 
	 * @param outJar
	 *            Jar file to save modifications to.
	 */
	public void saveJar(File outJar) {
		try {
			jarData.save(outJar);
		} catch (IOException e) {
			logging.error(e);
		}
	}

	/**
	 * Opens a class in the gui.
	 * 
	 * @param nodeName
	 *            Name of the class to open. Format is
	 *            <i>com/example/MyClass</i>.
	 */
	public ClassDisplayPanel selectClass(String nodeName) {
		return selectClass(jarData.classes.get(nodeName));
	}

	/**
	 * Opens a class in the gui.
	 * 
	 * @param cn
	 *            Node to open.
	 */
	public ClassDisplayPanel selectClass(ClassNode cn) {
		try {
			if (cn == null) {
				throw new RuntimeException("Node cannot be null!");
			}

			ClassDisplayPanel cdp = ui.openClass(cn);
			bus.post(new EClassSelect(cdp));
			return cdp;
		} catch (Exception e) {
			logging.error(e);
		}
		return null;
	}

	/**
	 * Launch with args parsed by Picocli.
	 * 
	 * @param args
	 *            Command line arguments containing optional arguments.
	 */
	public static void main(String[] args) {
		LaunchParams params = new LaunchParams();
		CommandLine.call(params, System.out, args);
		INSTANCE.setup(params);
	}

}