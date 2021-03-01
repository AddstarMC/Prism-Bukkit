package me.botsko.prism.config;

import io.leangen.geantyref.TypeToken;
import net.kyori.adventure.serializer.configurate4.ConfigurateComponentSerializer;
import org.spongepowered.configurate.ConfigurateException;
import org.spongepowered.configurate.ConfigurationNode;
import org.spongepowered.configurate.ConfigurationOptions;
import org.spongepowered.configurate.yaml.NodeStyle;
import org.spongepowered.configurate.yaml.YamlConfigurationLoader;

import java.nio.file.Path;
import java.text.DateFormat;
import java.util.Date;

/**
 * Created for Prism.
 *
 * @author Narimm on 22/02/2021
 * @since 2.1.8
 */
public class ConfigHandler {

    private static final YamlConfigurationLoader.Builder builder;
    private PrismConfig config;
    private ConfigurationNode main;
    private ConfigurationNode dataSourceConfig;

    static {
        ConfigurateComponentSerializer.Builder serializerBuilder = ConfigurateComponentSerializer.builder();
        ConfigurateComponentSerializer serializer = serializerBuilder.build();
        ConfigurationOptions options =  YamlConfigurationLoader.builder().defaultOptions()
                .header("Prism Config : Generated: " + DateFormat.getInstance().format(new Date()))
                .serializers(serializer.serializers());
        builder = YamlConfigurationLoader.builder().defaultOptions(options).nodeStyle(NodeStyle.BLOCK).indent(2);
    }

    public void loadConfiguration(Path path) {
        try {
            main = builder.path(path).build().load();
            config = main.node("prism").get(TypeToken.get(PrismConfig.class),new PrismConfig());
            dataSourceConfig = main.node("datasource");
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

    public PrismConfig getConfig() {
        return config;
    }
}
