CREATE SCHEMA IF NOT EXISTS bilbasen
DEFAULT CHARACTER SET utf8mb4;
USE bilbasen;

CREATE TABLE forhandler (
  forhandler_id INT AUTO_INCREMENT PRIMARY KEY,
  logo_link     VARCHAR(300) NOT NULL UNIQUE,
  lokation      VARCHAR(150),
  scraped_at    DATE NOT NULL
);

CREATE TABLE biler (
  car_id         INT PRIMARY KEY,
  model         VARCHAR(100),
  forhandler_id INT NULL,
  aargang       INT,
  km            INT,
  raekkevidde   INT NULL,
  link          VARCHAR(500),
  scraped_at    DATE NOT NULL,
  CONSTRAINT fk_biler_forhandler
    FOREIGN KEY (forhandler_id) REFERENCES forhandler(forhandler_id)
);

CREATE TABLE bil_priser (
  pris_id    INT AUTO_INCREMENT PRIMARY KEY,
  car_id      INT NOT NULL,
  pris       INT NOT NULL,
  sold       BOOLEAN NOT NULL DEFAULT FALSE,
  scraped_at DATE NOT NULL,
  CONSTRAINT fk_priser_bil
    FOREIGN KEY (car_id) REFERENCES biler(car_id)
);
