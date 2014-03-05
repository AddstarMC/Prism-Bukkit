package com.helion3.pste.api;

import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.HttpURLConnection;
import java.net.URL;

import javax.xml.bind.DatatypeConverter;

import org.bukkit.craftbukkit.libs.com.google.gson.Gson;
import org.bukkit.craftbukkit.libs.com.google.gson.GsonBuilder;


/**
 * Handles basic PSTE.me API calls
 * @author botskonet
 *
 */
public class PsteApi {
	
	private final String apiUsername;
	private final String apiKey;
	private final String apiUrl = "https://pste.me/api/v1/";

	
	/**
	 * 
	 * @param apiUsername
	 * @param apiKey
	 */
	public PsteApi( String apiUsername, String apiKey ){
		this.apiUsername = apiUsername;
		this.apiKey = apiKey;
	}
	
	
	/**
	 * Create a new paste
	 * @param paste
	 * @throws IOException
	 */
	public Results createPaste( Paste paste ) throws IOException{
		
		Gson gson = new GsonBuilder().disableHtmlEscaping().create();
		String jsonPayload = gson.toJson(paste);
		
		URL url = new URL( apiUrl+"/paste/" );
		
		HttpURLConnection connection = (HttpURLConnection) url.openConnection();
		connection.setRequestMethod("POST");
		connection.setRequestProperty("Content-Length", Integer.toString(jsonPayload.getBytes().length));
		connection.setRequestProperty("Content-Language", "en-US");
		connection.setRequestProperty("Authorization", "Basic " + 
				DatatypeConverter.printBase64Binary((apiUsername + ":" + apiKey).getBytes()));
		connection.setUseCaches(false);
		connection.setDoInput(true);
		connection.setDoOutput(true);
		DataOutputStream wr = new DataOutputStream(connection.getOutputStream());
		wr.writeBytes( jsonPayload );
		wr.flush();
		wr.close();
		
		// Read response
		InputStream is = connection.getInputStream();
		BufferedReader rd = new BufferedReader(new InputStreamReader(is));
		String json = readAll(rd);
		
		Results response = gson.fromJson(json, Results.class);
		return response;
		
	}
	
	
	/**
	 * 
	 * @param rd
	 * @return
	 * @throws IOException
	 */
	private String readAll(Reader rd) throws IOException {
	    StringBuilder sb = new StringBuilder();
	    int cp;
	    while ((cp = rd.read()) != -1) {
	      sb.append((char) cp);
	    }
	    return sb.toString();
	}
}