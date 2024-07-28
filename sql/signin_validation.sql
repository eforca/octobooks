SELECT username
FROM logins
WHERE username = ?username
  AND password = ?password;
