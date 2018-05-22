package me.botsko.prism.database.mysql;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Locale;
import java.util.function.BiConsumer;

import org.apache.commons.lang.Validate;

import me.botsko.prism.Prism;

public class IdMapQuery {
	private String prefix;

	private static final String toIds = "SELECT block_id, block_subid FROM <prefix>id_map WHERE material=? AND state=? LIMIT 1;";
	private static final String toMat = "SELECT material, state FROM <prefix>id_map WHERE block_id=? AND block_subid=? LIMIT 1;";
	private static final String map = "INSERT INTO <prefix>id_map(material, state, block_id, block_subid) VALUES (?, ?, ?, ?);";
	private static final String automap = "INSERT INTO <prefix>id_map(material, state) VALUES (?, ?);";
	private static final String repair = "UPDATE <prefix>id_map SET block_id=?, block_subid=? WHERE block_id=?;";
	private static final String unauto = "ALTER TABLE <prefix>id_map AUTO_INCREMENT=?;";
	
	public IdMapQuery() {
		prefix = Prism.config.getString("prism.mysql.prefix");
	}
	
	// hehehehehehehe
	private static final void noop() {
	}
	
	public void findMaterial(int block_id, int block_subid, BiConsumer<String, String> success) {
		findMaterial(block_id, block_subid, success, IdMapQuery::noop);
	}

	public void findMaterial(int block_id, int block_subid, BiConsumer<String, String> success, Runnable failure) {
		Validate.notNull(success, "Success callback cannot be null");
		Validate.notNull(failure, "Failure callback cannot be null (use findMaterial(int, int, BiConsumer)");
		
		String query = toMat.replace("<prefix>", prefix);

		try(Connection conn = Prism.dbc()) {
			try(PreparedStatement st = conn.prepareStatement(query)) {
				st.setInt(1, block_id);
				st.setInt(2, block_subid);
				try(ResultSet rs = st.executeQuery()) {
					if(rs.next())
						success.accept(rs.getString(1), rs.getString(2));
					else
						failure.run();
				}
			}
		} catch ( final SQLException e ) {
            Prism.log( "Database connection error: " + e.getMessage() );
            e.printStackTrace();
		}
	}
	
	public void findIds(String material, String state, BiConsumer<Integer, Integer> success) {
		findIds(material, state, success, IdMapQuery::noop);
	}

	public void findIds(String material, String state, BiConsumer<Integer, Integer> success, Runnable failure) {
		Validate.notNull(material, "Material cannot be null");
		Validate.notNull(state, "State cannot be null");
		Validate.notNull(success, "Success callback cannot be null");
		Validate.notNull(failure, "Failure callback cannot be null (use findIds(String, String, BiConsumer)");
		
		String query = toIds.replace("<prefix>", prefix);
		
		if(state.equals("0"))
			state = "";

		try(Connection conn = Prism.dbc()) {
			try(PreparedStatement st = conn.prepareStatement(query)) {
				st.setString(1, material.toLowerCase(Locale.ENGLISH));
				st.setString(2, state.toLowerCase(Locale.ENGLISH));
				try(ResultSet rs = st.executeQuery()) {
					if(rs.next())
						success.accept(rs.getInt(1), rs.getInt(2));
					else
						failure.run();
				}
			}
		} catch ( final SQLException e ) {
            Prism.log( "Database connection error: " + e.getMessage() );
            e.printStackTrace();
		}
	}
	
	public void map(String material, String state, int block_id, int block_subid) {
		Validate.notNull(material, "Material cannot be null");
		Validate.notNull(state, "State cannot be null");
		
		String query = map.replace("<prefix>", prefix);
		
		if(state.equals("0"))
			state = "";
		
		// Auto increment trouble. "0" in MYSQL can also mean "I am a placeholder and fill me in please", which is annoying here.
		if(block_id == 0) {
			query = repair.replace("<prefix>", prefix);
			int auto_id = mapAutoId(material, state);
			
			try(Connection conn = Prism.dbc()) {
				try(PreparedStatement st = conn.prepareStatement(query)) {
					st.setInt(1, block_id);
					st.setInt(2, block_subid);
					st.setInt(3, auto_id);
					
					st.executeUpdate();
				}
				
				// If the statement above fails, we can't roll back the auto increment without risk of collision (and making things worse)
				// Don't attempt to run in that case
				try(PreparedStatement st = conn.prepareStatement(unauto.replace("<prefix>", prefix))) {
					st.setInt(1, auto_id);
					
					st.executeUpdate();
				}
			} catch ( final SQLException e ) {
	            Prism.log( "Database connection error: " + e.getMessage() );
	            e.printStackTrace();
			}
		}
		else
			try(Connection conn = Prism.dbc()) {
				try(PreparedStatement st = conn.prepareStatement(query)) {
					st.setString(1, material.toLowerCase(Locale.ENGLISH));
					st.setString(2, state.toLowerCase(Locale.ENGLISH));
					st.setInt(3, block_id);
					st.setInt(4, block_subid);
					
					st.executeUpdate();
				}
			} catch ( final SQLException e ) {
	            Prism.log( "Database connection error: " + e.getMessage() );
	            e.printStackTrace();
			}
	}
	
	public int mapAutoId(String material, String state) {
		Validate.notNull(material, "Material cannot be null");
		Validate.notNull(state, "State cannot be null");
		
		String query = automap.replace("<prefix>", prefix);
		
		if(state.equals("0"))
			state = "";
		
		try(Connection conn = Prism.dbc()) {
			try(PreparedStatement st = conn.prepareStatement(query, Statement.RETURN_GENERATED_KEYS)) {
				st.setString(1, material.toLowerCase(Locale.ENGLISH));
				st.setString(2, state.toLowerCase(Locale.ENGLISH));
				
				st.executeUpdate();
				
				ResultSet rs = st.getGeneratedKeys();
				if(rs.next())
					return rs.getInt(1);
			}
		} catch ( final SQLException e ) {
            Prism.log( "Database connection error: " + e.getMessage() );
            e.printStackTrace();
		}
		
		return 0;
	}
}
