CREATE TABLE IF NOT EXISTS `prism_actions`
(
    `action_id` int(10) unsigned NOT NULL AUTO_INCREMENT,
    `action`    varchar(25)      NOT NULL,
    PRIMARY KEY (`action_id`),
    UNIQUE KEY `action` (`action`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8;
CREATE TABLE IF NOT EXISTS `prism_data`
(
    `id`              bigint(20) unsigned NOT NULL AUTO_INCREMENT,
    `epoch`           int(10) unsigned    NOT NULL,
    `action_id`       int(10) unsigned    NOT NULL,
    `player_id`       int(10) unsigned    NOT NULL,
    `world_id`        int(10) unsigned    NOT NULL,
    `x`               int(11)             NOT NULL,
    `y`               int(11)             NOT NULL,
    `z`               int(11)             NOT NULL,
    `block_id`        mediumint(5) DEFAULT NULL,
    `block_subid`     mediumint(5) DEFAULT NULL,
    `old_block_id`    mediumint(5) DEFAULT NULL,
    `old_block_subid` mediumint(5) DEFAULT NULL,
    PRIMARY KEY (`id`),
    KEY `epoch` (`epoch`),
    KEY `location` (`world_id`, `x`, `z`, `y`, `action_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8;
CREATE TABLE IF NOT EXISTS `prism_data_extra`
(
    `extra_id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
    `data_id`  bigint(20) unsigned NOT NULL,
    `data`     text                NULL,
    `te_data`  text                NULL,
    PRIMARY KEY (`extra_id`),
    KEY 'data_id' (`data_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8;
ALTER TABLE `prism_data_extra`
    ADD CONSTRAINT `prism_data_extra_ibfk_1` FOREIGN KEY (`data_id`) REFERENCES `prism_data` (`id`) ON DELETE CASCADE ON UPDATE NO ACTION;
CREATE TABLE IF NOT EXISTS `prism_meta`
(
    `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
    `k`  varchar(25)      NOT NULL,
    `v`  varchar(255)     NOT NULL,
    PRIMARY KEY (`id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8;
CREATE TABLE IF NOT EXISTS `prism_players`
(
    `player_id`   int(10) unsigned NOT NULL AUTO_INCREMENT,
    `player`      varchar(255)     NOT NULL,
    `player_uuid` binary(16)       NOT NULL,
    PRIMARY KEY (`player_id`),
    UNIQUE KEY `player` (`player`),
    UNIQUE KEY `player_uuid` (`player_uuid`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8;
CREATE TABLE IF NOT EXISTS `prism_worlds`
(
    `world_id` int(10) unsigned NOT NULL AUTO_INCREMENT,
    `world`    varchar(255)     NOT NULL,
    PRIMARY KEY (`world_id`),
    UNIQUE KEY `world` (`world`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8;
CREATE TABLE IF NOT EXISTS `prism_id_map`
(
    `material`    varchar(63)  NOT NULL,
    `state`       varchar(255) NOT NULL,
    `block_id`    mediumint(5) NOT NULL AUTO_INCREMENT,
    `block_subid` mediumint(5) NOT NULL DEFAULT 0,
    PRIMARY KEY (`material`, `state`),
    UNIQUE KEY (`block_id`, `block_subid`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8;