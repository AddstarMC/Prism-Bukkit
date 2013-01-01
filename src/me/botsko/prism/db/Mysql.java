package me.botsko.prism.db;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;

public class Mysql {
	
	String mysql_user;
	String mysql_pass;
	String mysql_hostname;
	String mysql_database;
	String mysql_port;
	
	protected Connection conn = null;
	
	
	/**
	 * 
	 * @param mysql_user
	 * @param mysql_pass
	 * @param mysql_hostname
	 * @param mysql_database
	 * @param mysql_port
	 */
	public Mysql( String mysql_user, String mysql_pass, String mysql_hostname, String mysql_database, String mysql_port ){
		this.mysql_user = mysql_user;
		this.mysql_pass = mysql_pass;
		this.mysql_hostname = mysql_hostname;
		this.mysql_database = mysql_database;
		this.mysql_port = mysql_port;
	}
	
	
	/**
     * Connects to the MySQL database
	 * @throws ClassNotFoundException 
	 * @throws IllegalAccessException 
	 * @throws InstantiationException 
	 * @return true if we successfully connected to the db.
     */
	public Connection getConn(){
		
		String dsn = ("jdbc:mysql://" + mysql_hostname + ":" + mysql_port + "/" + mysql_database);

		try {
			if (conn == null || conn.isClosed() || !conn.isValid(1)) {
				if (conn != null && !conn.isClosed()) {
					try {
						conn.close();
					} catch (Exception e) {
					}
				}
				if ((mysql_user.equalsIgnoreCase("")) && (mysql_pass.equalsIgnoreCase(""))){
					conn = DriverManager.getConnection(dsn);
				} else {
					conn = DriverManager.getConnection(dsn, mysql_user, mysql_pass);
				}
				if(conn == null || conn.isClosed()){
					return null;
				}
			}
		} catch (SQLException e) {
//			log("Error could not Connect to db " + dsn + ": " + e.getMessage());
		}
		return this.conn;
	}
}