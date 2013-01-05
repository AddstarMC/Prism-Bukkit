package me.botsko.prism.appliers;

public interface Previewable {
	
	/**
	 * 
	 * @return
	 */
	public ApplierResult preview();
	
	
	/**
	 * 
	 */
	public void cancel_preview();
	
	
	/**
	 * 
	 */
	public void apply_preview();
	
	
	/**
	 * 
	 * @return
	 */
	public ApplierResult apply();
}
