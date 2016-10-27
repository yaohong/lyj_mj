create database if not exists qp;
use qp;
create table if not exists `account` (
	`user_id` bigint(20) NOT NULL AUTO_INCREMENT,
	`acc` varchar(64) NOT NULL,
	`gold` bigint(20) DEFAULT 10000,
	`nickname` varchar(32) DEFAULT '',
	`avatar_url` varchar(128) DEFAULT '',
	`time` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
	PRIMARY KEY (`user_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8, AUTO_INCREMENT=100000;