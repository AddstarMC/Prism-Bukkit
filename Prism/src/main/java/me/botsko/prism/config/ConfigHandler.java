package me.botsko.prism.config;

import io.leangen.geantyref.TypeToken;
import me.botsko.prism.PrismLogHandler;
import me.botsko.prism.database.PrismDataSource;
import net.kyori.adventure.serializer.configurate4.ConfigurateComponentSerializer;
import org.spongepowered.configurate.ConfigurateException;
import org.spongepowered.configurate.ConfigurationNode;
import org.spongepowered.configurate.ConfigurationOptions;
import org.spongepowered.configurate.serialize.SerializationException;
import org.spongepowered.configurate.yaml.NodeStyle;
import org.spongepowered.configurate.yaml.YamlConfigurationLoader;

import java.lang.reflect.InvocationTargetException;
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
            PrismLogHandler.warn(e.getMessage(),e);
            e.printStackTrace();
        }
    }

    public <T> void applyDataSourceConfig(PrismDataSource<T> dataSource) {
        try {
            dataSourceConfig.set(TypeToken.get(dataSource.getConfigurationClass()),dataSource.getConfig());
        } catch (SerializationException e) {
            PrismLogHandler.warn(e.getMessage(),e);
        }
    }

    public void saveConfiguration(Path path) {
        YamlConfigurationLoader loader =  builder.path(path).build();
        try {
            main.node("prism").set(config);
            main.node("datasource").from(dataSourceConfig);
            loader.save(main);
        } catch (ConfigurateException e) {
            PrismLogHandler.warn(e.getMessage(),e);
        }
    }

    public ConfigurationNode getDataSourceConfig() {
        return dataSourceConfig;
    }

    public static <T> T getDataSourceConfig(Class<T> clazz,ConfigurationNode dataSourceConfig) {
        try {
            T object = clazz.getDeclaredConstructor().newInstance();
            return dataSourceConfig.get(TypeToken.get(clazz),object);
        } catch (InstantiationException | IllegalAccessException | NoSuchMethodException | InvocationTargetException e) {
            PrismLogHandler.warn(e.getMessage(),e);
            return null;
        } catch (ConfigurateException e) {
            PrismLogHandler.warn(e.getMessage(),e);
            try {
                return clazz.newInstance();
            } catch (InstantiationException | IllegalAccessException instantiationException) {
                PrismLogHandler.warn(e.getMessage(),e);
                return null;
            }
        }
    }

    public PrismConfig getConfig() {
        return config;
    }
}
