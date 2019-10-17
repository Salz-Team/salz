import createRepo from '~/api/repo';
console.log(process.env.apiURL);
export default (ctx, inject) => {
  const repoWithAxios = createRepo(ctx.$axios);
  inject('framesRepo', repoWithAxios('/frames'));
};
