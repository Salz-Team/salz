<template>
  <nav
    class="navbar header has-shadow is-primary"
    role="navigation"
    aria-label="main navigation"
  >
    <div class="navbar-brand">
      <a class="navbar-item" href="/">
        <img src="~assets/buefy.png" alt="Buefy" height="28" />
      </a>

      <div class="navbar-burger">
        test
        <span />
        <span />
        <span />
      </div>
    </div>
    <div class="navbar-menu">
      <ul class="navbar-start">
        <li v-for="(item, key) of items" :key="key">
          <nuxt-link :to="item.to" exact-active-class="is-active">
            <b-icon :icon="item.icon" /> {{ item.title }}
          </nuxt-link>
        </li>
      </ul>
      <ul class="navbar-end">
        <li><a>Sign up</a></li>
        <li><a>Login</a></li>
      </ul>
    </div>
  </nav>
</template>

<style lang="scss" scoped>
@import '~bulma/sass/utilities/_all';
@import '~assets/css/bulma-configs';

.navbar-menu {
  align-items: center;
}

ul {
  height: 100%;
  list-style: none;
  justify-content: space-between;
  align-items: center;

  li {
    display: flex;
    align-items: center;

    a {
      height: 100%;
      padding: 0 15px;
      display: flex;
      align-items: center;
      transition: all 0.2s ease-in-out;
    }

    a:link,
    a:visited {
      color: $light;
    }

    a:hover,
    a:focus,
    a.is-active {
      background: $primary-darker-10;
      color: $light;
    }
  }
}
</style>

<script charset="utf-8">
export default {
  data() {
    return {
      items: [
        {
          title: 'Home',
          icon: 'home',
          to: { name: 'index' }
        },
        {
          title: 'About',
          icon: 'book',
          to: { name: 'about' }
        },
        {
          title: 'Inspire',
          icon: 'lightbulb',
          to: { name: 'inspire' }
        },
        {
          title: 'Game',
          icon: 'gamepad',
          to: { name: 'game' }
        }
      ]
    };
  }
};
</script>
