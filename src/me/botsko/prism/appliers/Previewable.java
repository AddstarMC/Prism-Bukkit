package me.botsko.prism.appliers;

public interface Previewable {
	
	
	/**
	 * 
	 */
	public void setIsPreview(boolean is_preview);
	
	
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
