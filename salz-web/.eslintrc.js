module.exports = {
  root: true,
  env: {
    browser: true,
    node: true,
  },
  parserOptions: {
    parser: 'babel-eslint',
  },
  extends: [
    '@nuxtjs',
    'prettier',
    'prettier/vue',
    'plugin:prettier/recommended',
    'plugin:nuxt/recommended',
  ],
  plugins: ['prettier'],
  // add your custom rules here
  // reference: https://eslint.org/docs/rules/
  rules: {
    'new-cap': 'off',
    'no-extra-parens': 'warn',
    'no-unused-vars': 'warn',
    'prefer-const': 'warn',
    quotes: 'warn',
  },
};
