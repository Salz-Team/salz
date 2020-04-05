import { shallowMount } from '@vue/test-utils';
import Navbar from '@/components/Navbar.vue';

let wrapper;

beforeEach(() => {
  wrapper = shallowMount(Navbar, {
    computed: {
      isLoggedIn: () => true,
      token: () => 'sometoken',
      username: () => 'Player 1',
      id: () => 0,
    },
  });
});

afterEach(() => {
  wrapper.destroy();
});

describe('Navbar', () => {
  it("shows username as 'Player 1' when user is logged in", () => {
    expect(wrapper.find('.navbar-end > li').text()).toBe('Player 1');
  });

  it('shows Login button when user is not logged in', () => {
    wrapper = shallowMount(Navbar, {
      computed: {
        isLoggedIn: () => false,
        token: () => '',
        username: () => 'Player 1',
        id: () => 0,
      },
    });

    expect(wrapper.find('.navbar-end > li').text()).toBe('Login');
  });

  // it('toggles .active in .navbar-menu when running toggleNavbar', () => {
  //   expect(wrapper.find('.navbar-menu.active').exists()).toBe(false);
  //   wrapper.vm.toggleNavbar();
  //   expect(wrapper.find('.navbar-menu').classes()).toContain('active');
  // });
});
