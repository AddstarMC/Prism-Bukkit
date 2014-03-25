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
    public void preview();

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
    public void apply();
}