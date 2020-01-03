import { shallowMount } from '@vue/test-utils';
import FrameControl from '@/components/Game/FrameControl.vue';

let wrapper;

beforeEach(() => {
  wrapper = shallowMount(FrameControl, {
    computed: {
      currentFrameNumber: () => 50,
      lastFrame: () => 100,
      isPlaying: () => false
    }
  });
});

afterEach(() => {
  wrapper.destroy();
});

describe('FrameControl', () => {
  it('shows 1x speed by default', () => {
    expect(wrapper.find('.speed-button').text()).toBe('1x');
  });

  it('has that value of range slider is 50', () => {
    expect(wrapper.find('#frame-progress').element.value).toBe('50');
  });
});
