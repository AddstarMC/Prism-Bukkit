package me.botsko.prism.config;

import me.botsko.prism.database.PrismDatabaseFactory;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.spongepowered.configurate.serialize.SerializationException;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.Assert.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Created for Prism.
 *
 * @author Narimm on 23/02/2021
 * @since 2.1.8
 */
class ConfigHandlerTest {

    @Test
    void loadConfiguration() {
        try {
            Path path = Files.createTempFile(null, ".yml");
            ConfigHandler configHandler = new ConfigHandler();
            configHandler.loadConfiguration(path);
            PrismConfig config = configHandler.getConfig();
            Assertions.assertFalse(config.debug);
            Assertions.assertFalse(config.alertConfig.illegalCommands.enabled);
            config.debug = true;
            Assertions.assertTrue(configHandler.getConfig().debug);
            configHandler.saveConfiguration(path);
            File file = path.toFile();
            Assertions.assertTrue(file.exists());
            ConfigHandler configHandler2 = new ConfigHandler();
            configHandler2.loadConfiguration(path);
            PrismConfig config2 = configHandler2.getConfig();
            Assertions.assertTrue(config2.debug);
            Assertions.assertEquals(config.applierConfig.additionalNotifyRadius,
                    config2.applierConfig.additionalNotifyRadius);
            assertFalse(configHandler.getDataSourceConfig().hasChild("type"));
            try {
                PrismDatabaseFactory.createDefaultConfig(configHandler.getDataSourceConfig());
            }catch (SerializationException e){
                Assertions.fail(e.getMessage());
            }
            assertTrue(configHandler.getDataSourceConfig().hasChild("type"));
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}