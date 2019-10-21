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
        <li><b-icon icon="account" /> {{ username }}</li>
        <li @click="logout"><b-icon icon="logout" /> Logout</li>
      </ul>
      <ul v-else class="navbar-end">
        <li @click="login"><b-icon icon="login" /> Login</li>
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
import { parseJWT } from '../lib/jwt';

export default {
  data() {
    return {
      isLoggedIn: false,
      loginToken: window.localStorage.getItem('auth_token'),
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
      ],
      username: null
    };
  },
  mounted() {
    if (this.loginToken !== null) {
      this.isLoggedIn = true;
      const authdata = parseJWT(this.loginToken);
      sessionStorage.setItem('username', authdata.login);
      sessionStorage.setItem('id', authdata.id);

      this.username = sessionStorage.getItem('username');
      this.id = sessionStorage.getItem('id');
    }
  },
  methods: {
    login(event) {
      window.location.href = process.env.apiurl + '/login?client=web';
    },
    logout(event) {
      this.isLoggedIn = false;
      sessionStorage.removeItem('username');
      localStorage.removeItem('auth_token');
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
