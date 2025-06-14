-- Q1 What is the distribution of Netflix content types (movies vs. TV shows)?
SELECT 
  type,
  COUNT(*) AS count,
  ROUND(COUNT(*) * 100.0 / (SELECT COUNT(*) FROM titles), 2) AS percentage
FROM titles
GROUP BY type
ORDER BY count DESC;

-- Q2 Which countries produce the most content in each genre?
SELECT
  outer_q.genre_name,
  outer_q.country_name,
  outer_q.content_count
FROM
(
  SELECT
    g.genre_name,
    c.country_name,
    COUNT(*) AS content_count,
    RANK() OVER (PARTITION BY g.genre_name ORDER BY COUNT(*) DESC) AS rank_
  FROM
    titles t
    JOIN title_genres tg ON t.title_id = tg.title_id
    JOIN genres g ON tg.genre_id = g.genre_id
    JOIN title_countries tc ON t.title_id = tc.title_id
    JOIN countries c ON tc.country_id = c.country_id
  WHERE t.type != 'nan' AND c.country_name != 'nan'
  GROUP BY g.genre_name, c.country_name
) AS outer_q
WHERE outer_q.rank_ = 1
ORDER BY outer_q.genre_name;

-- Q3 Has the ratio of movies to TV shows changed throughout the dataset's date_added time period?
WITH content_by_year AS (
  SELECT
    YEAR(date_added) AS year,
    SUM(CASE WHEN type = 'Movie' THEN 1 ELSE 0 END) AS movie_count,
    SUM(CASE WHEN type = 'TV Show' THEN 1 ELSE 0 END) AS tv_show_count
  FROM titles
  WHERE type IS NOT NULL
  GROUP BY YEAR(date_added)
)
SELECT
  year,
  movie_count,
  tv_show_count,
  ROUND(movie_count * 1.0 / (movie_count + tv_show_count), 2) AS movie_ratio,
  ROUND(tv_show_count * 1.0 / (movie_count + tv_show_count), 2) AS tv_show_ratio
FROM content_by_year
ORDER BY year;

-- Q4 Which genres have the highest/lowest average duration for movies?
SELECT
  g.genre_name,
  AVG(t.duration) AS avg_duration
FROM 
  titles t
  JOIN title_genres tg ON t.title_id = tg.title_id
  JOIN genres g ON tg.genre_id = g.genre_id
WHERE t.type = 'Movie'
GROUP BY g.genre_name
ORDER BY avg_duration DESC, avg_duration ASC;

-- Q5 What are the most popular ratings for each genre?
SELECT
  inner_q.genre_name,
  inner_q.rating_value,
  inner_q.rating_count
FROM
(
  SELECT
    g.genre_name,
    r.rating_value,
    COUNT(*) AS rating_count,
    RANK() OVER (PARTITION BY g.genre_name ORDER BY COUNT(*) DESC) AS rank_
  FROM
    titles t
    JOIN title_genres tg ON t.title_id = tg.title_id
    JOIN genres g ON tg.genre_id = g.genre_id
    LEFT JOIN ratings r ON t.rating_id = r.rating_id
  WHERE r.rating_value IS NOT NULL
  GROUP BY g.genre_name, r.rating_value
) AS inner_q
WHERE inner_q.rank_ = 1
ORDER BY inner_q.genre_name;

-- Q6 What is the ratio of international content versus US? Does it differ by genre? By content type?
-- Overall ratio of international vs US content
WITH country_counts AS (
  SELECT
    c.country_name,
    COUNT(*) AS content_count
  FROM
    titles t
    JOIN title_countries tc ON t.title_id = tc.title_id
    JOIN countries c ON tc.country_id = c.country_id
  GROUP BY c.country_name
)
SELECT
  ROUND(SUM(CASE WHEN country_name != 'United States' THEN content_count ELSE 0 END) * 1.0 / SUM(content_count), 2) AS international_ratio,
  ROUND(SUM(CASE WHEN country_name = 'United States' THEN content_count ELSE 0 END) * 1.0 / SUM(content_count), 2) AS us_ratio
FROM country_counts;

-- Ratio by genre
SELECT
  g.genre_name,
  ROUND(SUM(CASE WHEN c.country_name != 'United States' THEN 1 ELSE 0 END) * 1.0 / COUNT(*), 2) AS international_ratio,
  ROUND(SUM(CASE WHEN c.country_name = 'United States' THEN 1 ELSE 0 END) * 1.0 / COUNT(*), 2) AS us_ratio
FROM
  titles t
  JOIN title_genres tg ON t.title_id = tg.title_id
  JOIN genres g ON tg.genre_id = g.genre_id
  JOIN title_countries tc ON t.title_id = tc.title_id
  JOIN countries c ON tc.country_id = c.country_id
GROUP BY g.genre_name
ORDER BY international_ratio DESC;

-- Ratio by content type  
SELECT
  t.type,
  ROUND(SUM(CASE WHEN c.country_name != 'United States' THEN 1 ELSE 0 END) * 1.0 / COUNT(*), 2) AS international_ratio,
  ROUND(SUM(CASE WHEN c.country_name = 'United States' THEN 1 ELSE 0 END) * 1.0 / COUNT(*), 2) AS us_ratio
FROM
  titles t
  JOIN title_countries tc ON t.title_id = tc.title_id
  JOIN countries c ON tc.country_id = c.country_id
GROUP BY t.type
ORDER BY international_ratio DESC;

-- Q7 Which actors appear most frequently in content on Netflix?
SELECT
  c.cast_name,
  COUNT(*) AS appearance_count
FROM
  casts c
  JOIN title_casts tc ON c.cast_id = tc.cast_id
WHERE c.cast_name != 'nan'
GROUP BY c.cast_name
ORDER BY appearance_count DESC
LIMIT 10;

-- Q8 Is there a pattern in the types of content added during different months?
WITH content_by_month AS (
  SELECT
    MONTH(date_added) AS month,
    SUM(CASE WHEN type = 'Movie' THEN 1 ELSE 0 END) AS movie_count,
    SUM(CASE WHEN type = 'TV Show' THEN 1 ELSE 0 END) AS tv_show_count
  FROM titles
  GROUP BY MONTH(date_added)
)
SELECT
  month,
  movie_count,
  tv_show_count,
  ROUND(movie_count * 1.0 / (movie_count + tv_show_count), 2) AS movie_ratio,
  ROUND(tv_show_count * 1.0 / (movie_count + tv_show_count), 2) AS tv_show_ratio
FROM content_by_month
ORDER BY month;

-- Q9 Who are the most common actor-director groupings in the database?
WITH actor_directors AS (
  SELECT
    c.cast_name AS actor,
    d.director_name AS director,
    COUNT(*) AS count
  FROM
    title_casts tc
    JOIN casts c ON tc.cast_id = c.cast_id
    JOIN titles t ON tc.title_id = t.title_id
    JOIN directors d ON t.director_id = d.director_id
    WHERE c.cast_name != 'nan' AND d.director_name != 'nan'
GROUP BY c.cast_name, d.director_name
)
SELECT
  actor,
  director,
  count,
  RANK() OVER (PARTITION BY actor ORDER BY count DESC) AS actor_rank,
  RANK() OVER (PARTITION BY director ORDER BY count DESC) AS director_rank
FROM actor_directors
ORDER BY count DESC
LIMIT 10;

-- Q10 Which directors specialize in specific genres?
WITH director_genres AS (
  SELECT
    d.director_name,
    g.genre_name,
    COUNT(*) AS content_count
  FROM
    titles t
    JOIN title_genres tg ON t.title_id = tg.title_id
    JOIN genres g ON tg.genre_id = g.genre_id
    JOIN directors d ON t.director_id = d.director_id
  WHERE d.director_name != 'nan' AND g.genre_name != 'nan'
  GROUP BY d.director_name, g.genre_name
)
SELECT
  director_name,
  genre_name,
  content_count,
  RANK() OVER (PARTITION BY director_name ORDER BY content_count DESC) AS genre_rank
FROM director_genres
ORDER BY content_count DESC
LIMIT 25;
