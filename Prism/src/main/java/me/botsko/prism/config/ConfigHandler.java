package me.botsko.prism.config;

import io.leangen.geantyref.TypeToken;
import me.botsko.prism.database.PrismDatabaseFactory;
import org.spongepowered.configurate.ConfigurateException;
import org.spongepowered.configurate.ConfigurationNode;
import org.spongepowered.configurate.yaml.NodeStyle;
import org.spongepowered.configurate.yaml.YamlConfigurationLoader;

import java.nio.file.Path;

/**
 * Created for Prism.
 *
 * @author Narimm on 22/02/2021
 * @since 2.1.8
 */
public class ConfigHandler {

    private YamlConfigurationLoader.Builder builder =  YamlConfigurationLoader
            .builder().nodeStyle(NodeStyle.BLOCK).indent(2);

    private PrismConfig config;
    private ConfigurationNode main;
    private ConfigurationNode dataSourceConfig;

    public void loadConfiguration(Path path) {
        YamlConfigurationLoader loader =  builder.path(path).build();
        try {
            main = loader.load();
            config = main.node("prism").get(TypeToken.get(PrismConfig.class),new PrismConfig());
            dataSourceConfig = main.node("datasource");
            PrismDatabaseFactory.createDefaultConfig(dataSourceConfig);
        } catch (ConfigurateException e) {
            e.printStackTrace();
        }
    }

    public void saveConfiguration(Path path) {
        YamlConfigurationLoader loader =  builder.path(path).build();
        try {
            main.node("prism").set(config);
            main.node("datasource").from(dataSourceConfig);
            loader.save(main);
        } catch (ConfigurateException e) {
            e.printStackTrace();
        }
    }

    public ConfigurationNode getDataSourceConfig() {
        return dataSourceConfig;
    }

    public PrismConfig getConfig(){
        return config;
    }
}
