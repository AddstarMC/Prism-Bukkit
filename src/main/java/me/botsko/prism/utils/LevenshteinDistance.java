package me.botsko.prism.utils;

import me.botsko.prism.Prism;

public class LevenshteinDistance {
	private static int minimum(int a, int b, int c) {
		return Math.min(Math.min(a, b), c);
	}

	private static int computeLevenshteinDistance(CharSequence str1,
			CharSequence str2) {
		int[][] distance = new int[str1.length() + 1][str2.length() + 1];

		for (int i = 0; i <= str1.length(); i++)
			distance[i][0] = i;
		for (int j = 1; j <= str2.length(); j++)
			distance[0][j] = j;

		for (int i = 1; i <= str1.length(); i++)
			for (int j = 1; j <= str2.length(); j++)
				distance[i][j] = minimum(
						distance[i - 1][j] + 1,
						distance[i][j - 1] + 1,
						distance[i - 1][j - 1]
								+ ((str1.charAt(i - 1) == str2.charAt(j - 1)) ? 0
										: 1));

		return distance[str1.length()][str2.length()];
	}

	public static String getClosestAction(String action) {
		String currentTopAction = "";
		int currentTopActionDistance = Integer.MAX_VALUE;
		int distance;
		String act;
		for (String possibleAct : Prism.getActionRegistry().listAll()) {
			act = possibleAct.toLowerCase();
			distance = LevenshteinDistance.computeLevenshteinDistance(action, act);
			currentTopAction = (currentTopAction.isEmpty() ? act : currentTopActionDistance > distance ? act : currentTopAction);
			currentTopActionDistance = (currentTopAction.equals(act) ? distance : currentTopActionDistance);
		}
		return currentTopAction;
	}
}
