package com.porpoise.codefornode.ui;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Image;
import java.awt.Point;
import java.awt.event.KeyEvent;
import java.io.File;
import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;

import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JTextField;
import javax.swing.UIManager;
import javax.swing.UIManager.LookAndFeelInfo;
import javax.swing.text.JTextComponent;

import com.google.common.base.Strings;

/**
 * Yet another clump of swing utilities
 */
public enum Swing {
    ;// uninstantiable

    enum OpenMode {
        DIRECTORIES_ONLY(JFileChooser.DIRECTORIES_ONLY), FILES_AND_DIRECTORIES(JFileChooser.FILES_AND_DIRECTORIES), FILES_ONLY(
                JFileChooser.FILES_ONLY);

        private final int value;

        OpenMode(final int mode) {
            this.value = mode;

        }

        public int getMode() {
            return this.value;
        }
    }

    // private void find(final Component owner, final JTextField target)
    // {
    // final OpenMode chooseFile = OpenMode.FILES_AND_DIRECTORIES;
    // find(owner, target, chooseFile);
    // }

    public static Preferences prefs() {
        return Preferences.userNodeForPackage(Swing.class);
    }

    public static JFrame show(final String title, final Component component) {
        return show(title, component, null);
    }

    public static JFrame show(final String title, final Component component, final Image icon) {
        final JFrame host = new JFrame(title);
        host.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        host.getContentPane().add(component, BorderLayout.CENTER);
        host.setIconImage(icon);
        host.pack();
        host.setVisible(true);
        return host;
    }

    public static void setDefaultTheme() {
        setTheme("Nimbus");
    }

    /**
     * @param name
     */
    public static void setTheme(final String name) {
        for (final LookAndFeelInfo info : UIManager.getInstalledLookAndFeels()) {
            if (name.equals(info.getName())) {
                try {
                    UIManager.setLookAndFeel(info.getClassName());
                } catch (final Exception e) {
                    throw new RuntimeException(e);
                }
                break;
            }
        }
    }

    public static void find(final Component owner, final JTextField target, final OpenMode chooseFile) {
        final File chosen = openFinder(owner, target.getText(), chooseFile);
        if (chosen != null) {
            final String path = chosen.getAbsolutePath();
            target.setText(path);
            savePrefs(target, path);
        }
    }

    public static File openFinder(final Component owner, final String text, final OpenMode mode) {
        final JFileChooser chooser = new JFileChooser();
        if (!Strings.isNullOrEmpty(text)) {
            final File file = new File(text);
            if (file.exists()) {
                chooser.setSelectedFile(file);
            }
        }
        chooser.setFileSelectionMode(mode.getMode());

        final int returnVal = chooser.showOpenDialog(owner);
        if (returnVal == JFileChooser.APPROVE_OPTION) {
            return chooser.getSelectedFile();
        }
        return null;
    }

    public static boolean isEnter(final KeyEvent evt) {
        return evt.getKeyCode() == KeyEvent.VK_ENTER;
    }

    public static boolean isDelete(final KeyEvent evt) {
        return evt.getKeyCode() == KeyEvent.VK_DELETE;
    }

    public static void savePrefs(final JTextComponent target) {
        savePrefs(target, target.getText());
    }

    public static void savePrefs(final Component target, final String path) {
        final String key = prefKeyForField(target);
        savePrefs(key, path);
    }

    /**
     * @param target
     * @return
     */
    public static String prefKeyForField(final Component target) {
        String name = target.getName();
        if (name == null) {
            final Point point = target.getLocation();
            name = String.format("FieldAt%sX%s", point.x, point.y);
        }
        final String key = name + ".text";
        return key;
    }

    /**
     * @param key
     * @param path
     */
    public static void savePrefs(final String key, final String path) {
        final Preferences prefs = prefs();

        if (Strings.isNullOrEmpty(key)) {
            throw new IllegalStateException("Can't save preferences without a key!");
        }
        prefs.put(key, path);
        try {
            prefs.flush();
        } catch (final BackingStoreException e) {
            throw new IllegalStateException(e);
        }
    }

    public static <C extends JTextComponent> C initFromPrefs(final C field, final String defaultValue) {
        final String key = prefKeyForField(field);
        field.setText(getUserPreference(key, defaultValue));
        return field;
    }

    public static String getUserPreference(final String key, final String defaultValue) {
        return prefs().get(key, defaultValue);
    }

    public static void showError(final Component owner, final String errorMsg) {
        JOptionPane.showMessageDialog(owner, errorMsg);
    }
}