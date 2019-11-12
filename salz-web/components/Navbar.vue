<template>
  <nav
    class="navbar header has-shadow is-primary"
    role="navigation"
    aria-label="main navigation"
  >
    <div class="navbar-brand">
      <a class="navbar-item" href="/">
        <img
          src="/assets/images/full-logo/full-logo@96h-white.png"
          alt="salz"
          height="28"
        />
      </a>

      <div class="navbar-burger" @click="toggleNavbar">
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
      <ul v-if="isLoggedIn" class="navbar-end">
        <li>
          <nuxt-link to="account">
            <b-icon icon="account" /> {{ username }}
          </nuxt-link>
        </li>
        <li>
          <a href="#" @click.prevent.stop="logout">
            <b-icon icon="logout" /> Logout
          </a>
        </li>
      </ul>
      <ul v-else class="navbar-end">
        <li>
          <a href="#" @click.prevent.stop="login">
            <b-icon icon="login" /> Login
          </a>
        </li>
      </ul>
    </div>
  </nav>
</template>

<style lang="scss" scoped>
@import '~bulma/sass/utilities/_all';
@import '~assets/css/bulma-configs';

.navbar-menu {
  align-items: center;

  &.active {
    display: block;
  }
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
      color: var(--body-bg-color);
    }
  }
}

ul.navbar-start {
  li {
    a:hover,
    a:focus,
    a.is-active {
      background: $primary-darker-10;
      color: var(--body-bg-color);
    }
  }
}

ul.navbar-end {
  li {
    background: none;
    color: var(--body-bg-color);
    cursor: pointer;
    padding: 0 15px;
    transition: all 0.2s ease-in-out;
    height: 100%;

    &:active,
    &:focus,
    &:hover {
      background: $primary-darker-10;
    }
  }
}

@media screen and (max-width: 1023px) {
  .navbar-menu {
    background-color: $primary;

    .navbar-start,
    .navbar-end {
      li {
        text-align: center;
        font-size: $size-4;

        & > a {
          width: 100%;
          line-height: $size-2;
        }
      }
    }
  }
}
</style>

<script charset="utf-8">
import { mapState } from 'vuex';

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
  },
  computed: {
    ...mapState({
      isLoggedIn: (state) => state.login.isLoggedIn,
      token: (state) => state.login.token,
      username: (state) => state.login.username,
      id: (state) => state.login.id
    })
  },
  methods: {
    login(event) {
      // HACK
      // Right now, what happens is that we redirect users to
      // the special endpoint /login?client=web on the API.
      // This is a workaround for the API complains about CORS
      // when Axios sends a get request to the default /login
      window.location.href = process.env.apiurl + '/login?client=web';
    },
    logout(event) {
      this.$store.dispatch('login/dropToken');
    },
    toggleNavbar() {
      const navMenu = document.querySelector('.navbar-menu');
      if (navMenu.classList.contains('active')) {
        navMenu.classList.remove('active');
      } else {
        navMenu.classList.add('active');
      }
    }
  }
};
</script>
