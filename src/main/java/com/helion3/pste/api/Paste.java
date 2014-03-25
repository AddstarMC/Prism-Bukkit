package com.helion3.pste.api;

public class Paste {

    private String access;
    private String created;
    private boolean currentUserMaySave;
    private String expires;
    private String expires_request;
    private String id;
    private String mode;
    private String name;
    private String paste;
    private String paste_type;
    private String slug;
    private boolean tags;
    private String username;

    public String getAccess() {
        return this.access;
    }

    public void setAccess(String access) {
        this.access = access;
    }

    public String getCreated() {
        return this.created;
    }

    public void setCreated(String created) {
        this.created = created;
    }

    public boolean getCurrentUserMaySave() {
        return this.currentUserMaySave;
    }

    public void setCurrentUserMaySave(boolean currentUserMaySave) {
        this.currentUserMaySave = currentUserMaySave;
    }

    public String getExpires() {
        return this.expires;
    }

    public void setExpires(String expires) {
        this.expires = expires;
    }

    public String getExpires_request() {
        return this.expires_request;
    }

    public void setExpires_request(String expires_request) {
        this.expires_request = expires_request;
    }

    public String getId() {
        return this.id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getMode() {
        return this.mode;
    }

    public void setMode(String mode) {
        this.mode = mode;
    }

    public String getName() {
        return this.name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getPaste() {
        return this.paste;
    }

    public void setPaste(String paste) {
        this.paste = paste;
    }

    public String getPaste_type() {
        return this.paste_type;
    }

    public void setPaste_type(String paste_type) {
        this.paste_type = paste_type;
    }

    public String getSlug() {
        return this.slug;
    }

    public void setSlug(String slug) {
        this.slug = slug;
    }

    public boolean getTags() {
        return this.tags;
    }

    public void setTags(boolean tags) {
        this.tags = tags;
    }

    public String getUsername() {
        return this.username;
    }

    public void setUsername(String username) {
        this.username = username;
    }
}