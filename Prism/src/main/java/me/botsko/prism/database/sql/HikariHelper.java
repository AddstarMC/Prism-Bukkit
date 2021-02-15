package me.botsko.prism.database.sql;

import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.util.PropertyElf;
import me.botsko.prism.Prism;
import me.botsko.prism.PrismLogHandler;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.text.DateFormat;
import java.util.Date;
import java.util.Properties;
import java.util.Set;

/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by Narimm on 1/01/2021.
 */
public class HikariHelper {

    /**
     * Creates the Standard Hikari Properties file.
     * @param propFile File
     * @param dbConfig HikariConfig
     * @param skipCoreValue if true skips the jbdcurl and username and password.
     */
    public static void createPropertiesFile(File propFile, HikariConfig dbConfig, boolean skipCoreValue) {
        dbConfig.setPoolName("prism");
        Properties prop = new Properties();
        Set<String> keys = PropertyElf.getPropertyNames(HikariConfig.class);
        for (String k : keys) {
            if (skipCoreValue) {
                if ("jbdcUrl".equals(k) || "username".equals(k) || "password".equals(k)) {
                    continue;
                }
            }
            if ("dataSourceProperties".equals(k) || "healthCheckProperties".equals(k)) {
                continue;
            }
            Object out = PropertyElf.getProperty(k, dbConfig);
            if (out != null) {
                prop.setProperty(k, out.toString());
            }
        }
        Properties datasourceProps = dbConfig.getDataSourceProperties();
        for (String name : datasourceProps.stringPropertyNames()) {
            String val = datasourceProps.getProperty(name);
            if (val != null) {
                prop.setProperty("dataSource." + name, val);
            }
        }
        try {
            if (!propFile.getParentFile().exists() && !propFile.getParentFile().mkdirs()) {
                PrismLogHandler.log("Prism Directory couldn't be created");
            }
            OutputStream out = new FileOutputStream(propFile);
            prop.store(out, "Prism Hikari Datasource Properties for"
                    + " advanced database Configuration - Updated: "
                    + DateFormat.getInstance().format(new Date()));
            PrismLogHandler.log("Database Configuration saved to - " + propFile.getPath());
        } catch (IOException e) {
            PrismLogHandler.log("Could not save Hikari.properties - " + e.getMessage());
        }
    }

    /**
     * Saves the config to the default dataFolder.
     *
     * @param config HikariConfig
     * @param skipCoreValues boolean
     */
    public static void createPropertiesFile(HikariConfig config,boolean skipCoreValues) {
        File propFile = new File(Prism.getInstance().getDataFolder(), "hikari.properties");
        if (propFile.exists()) {
            HikariConfig old = new HikariConfig(propFile.getPath());
            config.copyStateTo(old);
        }
        createPropertiesFile(propFile,config,skipCoreValues);
    }
}
