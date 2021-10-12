package me.botsko.prism.database;

import me.botsko.prism.Prism;
import me.botsko.prism.PrismLogHandler;
import me.botsko.prism.config.ConfigHandler;
import me.botsko.prism.database.mysql.MySqlPrimConfig;
import me.botsko.prism.database.sql.PrismSqlConfigImpl;
import me.botsko.prism.database.sql.derby.DerbySqlConfig;
import org.bukkit.Bukkit;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.spongepowered.configurate.ConfigurationNode;
import org.spongepowered.configurate.serialize.SerializationException;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

class PrismDatabaseFactoryTest {

    @Test
    public void createDataSourceTest(){
        Path path = null;
        try {
            path = Files.createTempFile(null, ".yml");
        } catch (IOException e) {
            Assertions.fail(e.getMessage());
        }
        Prism mock = Mockito.mock(Prism.class);
        PrismLogHandler.setSuppressLogging(true);
        Mockito.when(mock.getDataFolder()).thenReturn(path.getParent().toFile());
        PrismLogHandler.setSuppressLogging(true);
        ConfigHandler configHandler = new ConfigHandler();
        configHandler.loadConfiguration(path);
        ConfigurationNode node = configHandler.getDataSourceConfig();
        try (
                MockedStatic<Prism> mocked = Mockito.mockStatic(Prism.class);
                MockedStatic<Bukkit> bukkitMocked = Mockito.mockStatic(Bukkit.class)
        ) {
            mocked.when(Prism::getInstance).thenReturn(mock);
            bukkitMocked.when(Bukkit::isPrimaryThread).thenReturn(false);
            PrismDatabaseFactory.createDefaultConfig(node);
            PrismDataSource<?> dataSource = null;
            dataSource = PrismDatabaseFactory.createDataSource(node);
            Assertions.assertSame(dataSource.getConfigurationClass(), DerbySqlConfig.class);
            node.node("type").set("null");
            dataSource = PrismDatabaseFactory.createDataSource(node);
            Assertions.assertSame(dataSource.getConfigurationClass(),PrismSqlConfigImpl.class);
            node.node("type").set("randomNamedDataSource");
            dataSource = PrismDatabaseFactory.createDataSource(node);
            Assertions.assertSame(dataSource.getConfigurationClass(), DerbySqlConfig.class);
            node.node("type").set("sqlite");
            dataSource = PrismDatabaseFactory.createDataSource(node);
            Assertions.assertSame(dataSource.getConfigurationClass(),PrismSqlConfigImpl.class);
            node.node("type").set("mysql");
            dataSource = PrismDatabaseFactory.createDataSource(node);
            Assertions.assertSame(dataSource.getConfigurationClass(), MySqlPrimConfig.class);
            node.removeChild("type");
            dataSource = PrismDatabaseFactory.createDataSource(node);
            Assertions.assertSame(dataSource.getConfigurationClass(),PrismSqlConfigImpl.class);
        }catch (SerializationException e){
            Assertions.fail(e.getMessage());
        }
    }


}