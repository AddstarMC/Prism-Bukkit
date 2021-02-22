package me.botsko.prism.config;

import io.leangen.geantyref.TypeToken;
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

    private ConfigurationNode main;
    private PrismConfig prismDefaults;
    private ConfigurationNode dataSourceConfig;

    public void loadConfiguration(Path path) {
        YamlConfigurationLoader loader =  YamlConfigurationLoader
                .builder().nodeStyle(NodeStyle.BLOCK).indent(4).path(path).build();
        try {
            main = loader.load();
            prismDefaults = main.node("prism").get(TypeToken.get(PrismConfig.class),new PrismConfig());
            dataSourceConfig = main.node("datasource");
        } catch (ConfigurateException e) {
            e.printStackTrace();
        }
    }

}
