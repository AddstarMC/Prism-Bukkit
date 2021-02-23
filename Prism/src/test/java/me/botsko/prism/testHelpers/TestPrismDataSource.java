package me.botsko.prism.testHelpers;

import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;
import com.zaxxer.hikari.pool.HikariPool;
import me.botsko.prism.Prism;
import me.botsko.prism.PrismLogHandler;
import me.botsko.prism.database.PrismDataSource;
import me.botsko.prism.database.sql.HikariHelper;
import me.botsko.prism.database.sql.derby.DerbySqlPrismDataSource;
import org.bukkit.configuration.ConfigurationSection;
import org.spongepowered.configurate.ConfigurationNode;
import org.spongepowered.configurate.serialize.SerializationException;

import java.util.Random;

/**
 * Created for the Prism-Bukkit Project.
 *
 * @author Narimm on
 */
public class TestPrismDataSource extends DerbySqlPrismDataSource {

    private static Random random = new Random();
    /**
     * Constructor.
     *
     * @param section Config
     */
    public TestPrismDataSource(ConfigurationNode section) {
        super(section);
        try {
            updateDefaultConfig(section);
        } catch (SerializationException e) {
            e.printStackTrace();
        }
    }

    @Override
    public PrismDataSource createDataSource() {
        dbConfig = new HikariConfig();
        dbConfig.setUsername("prism");
        dbConfig.setPassword("password");
        dbConfig.setJdbcUrl("jdbc:derby:memory:test" + random.nextInt(100) + ";create=true");
        setPrefix("prism_");
        try {
            if (Prism.getInstance() != null) {
                HikariHelper.createPropertiesFile(dbConfig,false);
            }
            database = new HikariDataSource(dbConfig);
            createSettingsQuery();
            return this;
        } catch (HikariPool.PoolInitializationException e) {
            PrismLogHandler.warn("Hikari Pool did not Initialize: " + e.getMessage());
            database = null;
        }
        return this;
    }
}


