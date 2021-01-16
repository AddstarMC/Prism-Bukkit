package me.botsko.prism.database.mysql;

import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.util.PropertyElf;
import me.botsko.prism.Prism;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.Properties;
import java.util.Set;

/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 1/01/2021.
 */
class HikariHelper {

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
                Prism.log("Prism Directory couldn't be created");
            }
            OutputStream out = new FileOutputStream(propFile);
            prop.store(out, "Prism Hikari Datasource Properties for"
                    + " advanced database Configuration");
            Prism.log("Database Configuration saved to - " + propFile.getPath());
        } catch (IOException e) {
            Prism.log("Could not save Hikari.properties - " + e.getMessage());
        }
    }
}
