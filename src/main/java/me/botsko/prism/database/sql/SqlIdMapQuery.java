package me.botsko.prism.database.sql;

import me.botsko.prism.Prism;
import me.botsko.prism.database.IdMapQuery;
import me.botsko.prism.database.PrismDataSource;
import me.botsko.prism.utils.IntPair;
import org.apache.commons.lang.Validate;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.SQLWarning;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

public class SqlIdMapQuery implements IdMapQuery {
    private static final String toIds =
            "SELECT block_id, block_subid FROM <prefix>id_map WHERE material=? AND state=? LIMIT 1;";
    private static final String toAllIds = "SELECT block_id, block_subid FROM <prefix>id_map WHERE material=?;";
    private static final String partialToAllIds = "SELECT block_id, block_subid FROM <prefix>id_map "
            + "WHERE material=? AND state LIKE ?";
    private static final String toMat = "SELECT material, state FROM <prefix>id_map "
            + "WHERE block_id=? AND block_subid=? LIMIT 1;";
    private static final String map = "INSERT INTO <prefix>id_map(material, state, block_id, block_subid) "
            + "VALUES (?, ?, ?, ?);";
    private static final String automap = "INSERT INTO <prefix>id_map(material, state) VALUES (?, ?);";
    private static final String repair = "UPDATE <prefix>id_map SET block_id=?, block_subid=? WHERE block_id=?;";
    private static final String unauto = "ALTER TABLE <prefix>id_map AUTO_INCREMENT=?;";
    private final String prefix;
    private final PrismDataSource dataSource;

    /**
     * Constructor
     * @param dataSource  PrismDataSource
     */
    public SqlIdMapQuery(PrismDataSource dataSource) {
        this.dataSource = dataSource;
        prefix = dataSource.getPrefix();

    }

    private static void noop() {
    }

    public void findMaterial(int blockId, int blockSubid, BiConsumer<String, String> success) {
        findMaterial(blockId, blockSubid, success, SqlIdMapQuery::noop);
    }

    /**
     * Find material and consume it
     * @param blockId int
     * @param blockSubid int
     * @param success BiConsumer
     * @param failure Runnable.
     */
    public void findMaterial(int blockId, int blockSubid, BiConsumer<String, String> success, Runnable failure) {
        Validate.notNull(success, "Success callback cannot be null");
        Validate.notNull(failure, "Failure callback cannot be null (use findMaterial(int, int, BiConsumer)");

        String query = toMat.replace("<prefix>", prefix);

        try (Connection conn = dataSource.getConnection()) {
            try (PreparedStatement st = conn.prepareStatement(query)) {
                st.setInt(1, blockId);
                st.setInt(2, blockSubid);
                try (ResultSet rs = st.executeQuery()) {
                    if (rs.next()) {
                        success.accept(rs.getString(1), rs.getString(2));
                    } else {
                        failure.run();
                    }
                }
            }
        } catch (final SQLException e) {
            Prism.warn("Database connection error: ", e);
        }
    }

    public void findIds(String material, String state, BiConsumer<Integer, Integer> success) {
        findIds(material, state, success, SqlIdMapQuery::noop);
    }

    /**
     * Find ids and consume.
     * @param material String
     * @param state state
     * @param success Consumer
     * @param failure Runnable
     */
    public void findIds(String material, String state, BiConsumer<Integer, Integer> success, Runnable failure) {
        Validate.notNull(material, "Material cannot be null");
        Validate.notNull(state, "State cannot be null");
        Validate.notNull(success, "Success callback cannot be null");
        Validate.notNull(failure, "Failure callback cannot be null (use findIds(String, String, BiConsumer)");

        String query = toIds.replace("<prefix>", prefix);

        if (state.equals("0")) {
            state = "";
        }

        try (Connection conn = dataSource.getConnection()) {
            try (PreparedStatement st = conn.prepareStatement(query)) {
                st.setString(1, material);
                st.setString(2, state);
                try (ResultSet rs = st.executeQuery()) {
                    if (rs.next()) {
                        success.accept(rs.getInt(1), rs.getInt(2));
                    } else {
                        failure.run();
                    }
                }
            }
        } catch (final SQLException e) {
            Prism.warn("Database connection error: ", e);
        }
    }

    public void findAllIds(String material, Consumer<List<IntPair>> success) {
        findAllIds(material, success, SqlIdMapQuery::noop);
    }

    /**
     * Find and consume
     * @param material String
     * @param success Consumer
     * @param failure Runnable
     */
    public void findAllIds(String material, Consumer<List<IntPair>> success, Runnable failure) {
        Validate.notNull(material, "Material cannot be null");
        Validate.notNull(success, "Success callback cannot be null");
        Validate.notNull(failure, "Failure callback cannot be null (use findAllIds(String, BiConsumer)");

        String query = toAllIds.replace("<prefix>", prefix);

        try (Connection conn = dataSource.getConnection()) {
            try (PreparedStatement st = conn.prepareStatement(query)) {
                st.setString(1, material);
                handleIdResult(st,success,failure);
            }
        } catch (final SQLException e) {
            Prism.warn("Database connection error: ", e);
            e.printStackTrace();
        }
    }

    public void findAllIdsPartial(String material, String stateLike, Consumer<List<IntPair>> success) {
        findAllIdsPartial(material, stateLike, success, SqlIdMapQuery::noop);
    }

    /**
     * Find partials.
     * @param material String
     * @param stateLike String
     * @param success Consume
     * @param failure Runnable
     */
    private void findAllIdsPartial(String material, String stateLike, Consumer<List<IntPair>> success,
                                  Runnable failure) {
        Validate.notNull(material, "Material cannot be null");
        Validate.notNull(success, "Success callback cannot be null");
        Validate.notNull(failure, "Failure callback cannot be null (use findAllIds(String, BiConsumer)");

        String query = partialToAllIds.replace("<prefix>", prefix);

        try (Connection conn = dataSource.getConnection()) {
            try (PreparedStatement st = conn.prepareStatement(query)) {
                st.setString(1, material);
                st.setString(2, stateLike);
                handleIdResult(st,success,failure);
            }
        } catch (final SQLException e) {
            Prism.warn("Database connection error: ", e);
            e.printStackTrace();
        }
    }

    private void handleIdResult(PreparedStatement st, Consumer<List<IntPair>> success, Runnable failure) throws SQLException{
        try (ResultSet rs = st.executeQuery()) {
            List<IntPair> ids = new ArrayList<>();

            while (rs.next()) {
                ids.add(new IntPair(rs.getInt(1), rs.getInt(2)));
            }

            if (!ids.isEmpty()) {
                success.accept(ids);
            } else {
                failure.run();
            }
        }
    }

    /**
     * Build map.
     * @param material String
     * @param state state
     * @param blockId id
     * @param blockSubid  subid
     */
    public void map(String material, String state, int blockId, int blockSubid) {
        Validate.notNull(material, "Material cannot be null");
        Validate.notNull(state, "State cannot be null");

        String query = map.replace("<prefix>", prefix);

        if (state.equals("0")) {
            state = "";
        }

        // Auto increment trouble. "0" in MYSQL can also mean "I am a placeholder and
        // fill me in please", which is annoying here.
        if (blockId == 0) {
            query = repair.replace("<prefix>", prefix);
            int autoId = mapAutoId(material, state);

            try (Connection conn = dataSource.getConnection()) {
                try (PreparedStatement st = conn.prepareStatement(query)) {
                    st.setInt(1, blockId);
                    st.setInt(2, blockSubid);
                    st.setInt(3, autoId);

                    st.executeUpdate();
                }

                // If the statement above fails, we can't roll back the auto increment without
                // risk of collision (and making things worse)
                // Don't attempt to run in that case
                try (PreparedStatement st = conn.prepareStatement(unauto.replace("<prefix>", prefix))) {
                    st.setInt(1, autoId);

                    st.executeUpdate();
                }
            } catch (final SQLException e) {
                Prism.warn("Database connection error: ", e);
                e.printStackTrace();
            }
        } else {
            try (Connection conn = dataSource.getConnection()) {
                try (PreparedStatement st = conn.prepareStatement(query)) {
                    st.setString(1, material);
                    st.setString(2, state);
                    st.setInt(3, blockId);
                    st.setInt(4, blockSubid);

                    st.executeUpdate();
                }
            } catch (final SQLException e) {
                Prism.warn("Database connection error: ", e);
                e.printStackTrace();
            }
        }
    }

    public int mapAutoId(String material, String state) {
        Validate.notNull(material, "Material cannot be null");
        Validate.notNull(state, "State cannot be null");

        String query = automap.replace("<prefix>", prefix);

        if (state.equals("0") || state.equals("[]")) {
            state = "";
        }

        try (Connection conn = dataSource.getConnection()) {
            try (PreparedStatement st = conn.prepareStatement(query, Statement.RETURN_GENERATED_KEYS)) {
                st.setString(1, material);
                st.setString(2, state);

                boolean success = st.executeUpdate() > 0;

                SQLWarning warning = st.getWarnings();

                while (warning != null) {
                    Prism.warn("sql Warning: " + warning.getMessage());
                    warning = warning.getNextWarning();
                }

                ResultSet rs = st.getGeneratedKeys();
                if (rs.next()) {
                    int autoInc = rs.getInt(1);

                    if (!success) {
                        Prism.log("Failed id map: material=" + material + ", " + "state=" + state);
                    }

                    return autoInc;
                }
            }
        } catch (final SQLException e) {
            Prism.warn("Database connection error: ", e);
            e.printStackTrace();
        }

        return 0;
    }
}
