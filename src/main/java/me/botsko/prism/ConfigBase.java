package me.botsko.prism;

import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.plugin.Plugin;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

public class ConfigBase {


    protected final Plugin plugin;
    protected FileConfiguration config;

    /**
     * Constructor.
     * @param plugin Prism
     */
    ConfigBase(Plugin plugin) {
        this.plugin = plugin;
    }

    public FileConfiguration getConfig() {
        config = plugin.getConfig();
        return config;
    }

    /**
     * Loads language configuration.
     *
     * @return FileConfig
     */
    @SuppressWarnings("unused")
    public FileConfiguration getLang(String langString) {

        String langFile = langString;
        if (langFile == null) {
            langFile = "en-us";
        }

        // Read the base config
        return loadConfig("languages/", langFile);

    }

    /**
     * Returns base directory for config.
     *
     * @return File
     */
    private File getDirectory() {
        return new File(plugin.getDataFolder() + "");
    }

    /**
     * Returns chosen filename with directory.
     *
     * @return File
     */
    private File getFilename(String filename) {
        return new File(getDirectory(), filename + ".yml");
    }

    /**
     * Loads the config.
     * @param defaultFolder default folder
     * @param filename filename
     * @return FileConfig.
     */
    private FileConfiguration loadConfig(String defaultFolder, String filename) {
        final File file = getFilename(filename);
        if (file.exists()) {
            return YamlConfiguration.loadConfiguration(file);
        } else {
            // Look for defaults in the jar
            final InputStream defConfigStream = plugin.getResource(defaultFolder + filename + ".yml");
            if (defConfigStream != null) {
                return YamlConfiguration.loadConfiguration(new InputStreamReader(defConfigStream));
            }
            return null;
        }
    }

    /**
     * Save the Config.
     * @param filename the file name to save as
     * @param config FileConfig to save
     */
    protected void saveConfig(String filename, FileConfiguration config) {
        final File file = getFilename(filename);
        try {
            config.save(file);
        } catch (final IOException e) {
            // Prism.log("Could not save the configuration file to "+file);
            // Throw exception
        }
    }

    /**
     * Write to file.
     * @param filename String
     * @param config FileConfig
     */
    protected void write(String filename, FileConfiguration config) {
            saveConfig(filename, config);
    }
}
