package me.botsko.prism.measurement;

import java.util.HashMap;

public class QueueStats {
	public int actionsRecorded = 0;
	public HashMap<Integer,Integer> perRunRecordingCounts = new HashMap<Integer,Integer>();
}