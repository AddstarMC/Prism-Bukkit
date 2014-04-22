package me.botsko.prism.purge;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

import me.botsko.prism.Prism;

public class PurgeChunkingUtil {

    /**
     * 
     * @param playername
     */
    public static int getMinimumPrimaryKey() {
        String prefix = Prism.config.getString("prism.mysql.prefix");
        int id = 0;
        Connection conn = null;
        PreparedStatement s = null;
        ResultSet rs = null;
        try {

            conn = Prism.dbc();
            s = conn.prepareStatement( "SELECT MIN(id) FROM " + prefix + "data" );
            s.executeQuery();
            rs = s.getResultSet();

            if( rs.first() ) {
                id = rs.getInt( 1 );
            }

        } catch ( final SQLException ignored ) {

        } finally {
            if( rs != null )
                try {
                    rs.close();
                } catch ( final SQLException ignored ) {}
            if( s != null )
                try {
                    s.close();
                } catch ( final SQLException ignored ) {}
            if( conn != null )
                try {
                    conn.close();
                } catch ( final SQLException ignored ) {}
        }
        return id;
    }

    /**
     * 
     * @param playername
     */
    public static int getMaximumPrimaryKey() {
        String prefix = Prism.config.getString("prism.mysql.prefix");
        int id = 0;
        Connection conn = null;
        PreparedStatement s = null;
        ResultSet rs = null;
        try {

            conn = Prism.dbc();
            s = conn.prepareStatement( "SELECT id FROM " + prefix + "data ORDER BY id DESC LIMIT 1;" );
            s.executeQuery();
            rs = s.getResultSet();

            if( rs.first() ) {
                id = rs.getInt( 1 );
            }

        } catch ( final SQLException ignored ) {

        } finally {
            if( rs != null )
                try {
                    rs.close();
                } catch ( final SQLException ignored ) {}
            if( s != null )
                try {
                    s.close();
                } catch ( final SQLException ignored ) {}
            if( conn != null )
                try {
                    conn.close();
                } catch ( final SQLException ignored ) {}
        }
        return id;
    }
}