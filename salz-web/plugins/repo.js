import createRepo from '~/api/repo';
export default (ctx, inject) => {
  const repoWithAxios = createRepo(ctx.$axios);
  inject('framesRepo', repoWithAxios('/frames'));
  inject('user', repoWithAxios('/user'));
};
