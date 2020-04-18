/**
 * @function parseJWT
 * @remarks Parses the given JWT token into an object
 * @param  {string} token JWT token
 * @return {object} JSONified payload from the JWT
 */
export function parseJWT(token) {
  const base64Payload = token.split('.')[1];
  const base64 = base64Payload.replace(/-/g, '+').replace(/_/g, '-');
  const jsonPayload = decodeURIComponent(
    atob(base64)
      .split('')
      .map(c => {
        return '%' + ('00' + c.charCodeAt(0).toString(16)).slice(-2);
      })
      .join(''),
  );

  return JSON.parse(jsonPayload);
}
