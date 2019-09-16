const api_baseurl = () => {
  switch (process.env.FRONTEND_ENV) {
    case "prod":
    case "docker-dev":
      return "http://salz-api:8080";
    case "dev":
    default: 
      return "http://localhost:8080";
  }
}

export default {
  mode: 'spa',
  /*
   ** Headers of the page
   */
  head: {
    title: process.env.npm_package_name || '',
    meta: [
      { charset: 'utf-8' },
      { name: 'viewport', content: 'width=device-width, initial-scale=1' },
      {
        hid: 'description',
        name: 'description',
        content: process.env.npm_package_description || ''
      }
    ],
    link: [{ rel: 'icon', type: 'image/x-icon', href: '/favicon.ico' }]
  },
  /*
   ** Customize the progress-bar color
   */
  loading: { color: '#fff' },
  /*
   ** Global CSS
   */
  css: [
    '@assets/css/bulma-configs.scss',
    '@assets/css/styles.scss',
    '@assets/css/bulma-local.scss'
  ],
  /*
   ** Plugins to load before mounting the App
   */
  plugins: ['~/plugins/repo'],
  /*
   ** Nuxt.js dev-modules
   */
  buildModules: [
    // Doc: https://github.com/nuxt-community/eslint-module
    '@nuxtjs/eslint-module'
  ],
  /*
   ** Nuxt.js modules
   */
  modules: [
    // Doc: https://buefy.github.io/#/documentation
    'nuxt-buefy',
    // Info: https://axios.nuxtjs.org/setup
    '@nuxtjs/axios',
    // Doc: https://markdown-it.github.io/markdown-it/
    '@nuxtjs/markdownit'
  ],
  axios: {
    // proxyHeaders: false
    baseURL: api_baseurl()
  },
  markdownit: {
    preset: 'default',
    html: true,
    typographer: true,
  },
  /*
   ** Build configuration
   */
  build: {
    /*
     ** You can extend webpack config here
     */
    extend(config, ctx) {}
  }
};
