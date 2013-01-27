package me.botsko.prism;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLConnection;

public class UpdateNotification {
	
	
	/**
	 * Convert alpha/beta/rc to numbers so we understand their ranking
	 * @param ver
	 * @return
	 */
	protected static String normalizeVersionString( String ver ){
		ver = ver.replace("Alpha", "1");
		ver = ver.replace("Beta", "2");
		ver = ver.replace("RC", "3");
		return ver.replaceAll( "[^\\d]", "" );
	}
	

	/**
	 * 
	 */
	public static String checkForNewerBuild( String clientBuild ){
		
		String originalClientBuild = clientBuild;
		
		if(clientBuild.equalsIgnoreCase("nightly")){
			return null;
		}
		
		try {

        	URLConnection yc = new URL("http://botsko.s3.amazonaws.com/Prism/versions.txt").openConnection();
    		BufferedReader in = new BufferedReader(new InputStreamReader(yc.getInputStream()));

    		String originalCurrentBuild = in.readLine();
    		String currentBuild = normalizeVersionString(originalCurrentBuild);
    		clientBuild = normalizeVersionString(clientBuild);

    		// If the current build is shorter, it has to be newer. 
    		if(currentBuild.length() < clientBuild.length()){
    			return notifyNewVersion( originalCurrentBuild, originalClientBuild );
    		}
    		
    		// Check each digit for greater values, because version
    		// numbers could always be different lengths.
			for(int i = 0; i< currentBuild.length(); i++){
				String currVerSub = currentBuild.substring(i,i+1);
				String clientVerSub = clientBuild.substring(i,i+1);
				if(Integer.parseInt(currVerSub) > Integer.parseInt(clientVerSub)){
					notifyNewVersion( originalCurrentBuild, originalClientBuild );
					return notifyNewVersion( originalCurrentBuild, originalClientBuild );
				}
			}
			
    		in.close();

		} catch (Exception e) {

		}
		return null;
	}
	
	
	/**
	 * 
	 * @param newBuild
	 */
	public static String notifyNewVersion( String newBuild, String currentBuild ){
		return "An update to Prism is available. Version: " + newBuild + ". You have " + currentBuild;
	}
}
