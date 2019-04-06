package me.botsko.prism.purge;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

import me.botsko.prism.Prism;
import me.botsko.prism.database.SelectIDQuery;

public class PurgeChunkingUtil {


	public static long getMinimumPrimaryKey() {
		SelectIDQuery query = Prism.getPrismDataSource().createSelectIDQuery();
		query.setMin();
		query.setParameters(null);
		query.setShouldGroup(false);
		return query.execute();
	}

	public static long getMaximumPrimaryKey() {
		SelectIDQuery query = Prism.getPrismDataSource().createSelectIDQuery();
		query.setMax();
		query.setParameters(null);
		query.setShouldGroup(false);
		return query.execute();
	}

}