--DROP DATABASE motorola;
CREATE DATABASE motorola;

\c motorola

CREATE TABLE IF NOT EXISTS DEVICE_PROFILES (
    radio_id int PRIMARY KEY,
    alias VARCHAR NOT NULL,
    location int
);

CREATE TABLE IF NOT EXISTS LOCATIONS (
    location_id SERIAL UNIQUE,
    device_profile_id int REFERENCES DEVICE_PROFILES(radio_id),
    location_name VARCHAR NOT NULL
);

-- not safe to execute multiple times
--ALTER TABLE DEVICE_PROFILES 
--ADD CONSTRAINT location_constraint 
--FOREIGN KEY (location) 
--REFERENCES LOCATIONS (location_id);

CREATE INDEX IF NOT EXISTS by_dp ON LOCATIONS (
    device_profile_id
);
