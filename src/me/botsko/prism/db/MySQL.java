package me.botsko.prism.db;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;

public class MySQL {
	
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
	public MySQL( String mysql_user, String mysql_pass, String mysql_hostname, String mysql_database, String mysql_port ){
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
		
		String dsn = ("jdbc:mysql://" + mysql_hostname + ":" + mysql_port);

		try {
			if (conn == null || conn.isClosed() || !conn.isValid(1)) {
				if (conn != null && !conn.isClosed()) {
					try {
						conn.close();
					} catch (Exception e){
						System.out.print("[Prism]: Database connection close error: " + e.getMessage());
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
			
			Statement st = conn.createStatement();
			st.executeUpdate("CREATE DATABASE IF NOT EXISTS `" + mysql_database + "`");
			st.executeUpdate("USE `" + mysql_database + "`");
			
		} catch (SQLException e) {
			System.out.print("[Prism]: Database connection error: " + e.getMessage());
		}
		return this.conn;
	}
}