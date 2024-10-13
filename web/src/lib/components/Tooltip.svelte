<script lang="ts">
	import type { AriaRole } from 'svelte/elements';
	import { Position } from '$lib/components/types/Tooltip';

  export let isActive: boolean = false;
	export let role: AriaRole | null = 'tooltip';
	export let position: Position = Position.BottomCenter;
	let className = '';
	export { className as class };

	$: computedClass = [className, position.toString()].join(' ');
</script>

{#if isActive}
  <div {role} class={computedClass}>
    <slot></slot>
  </div>
{/if}

<style>
div {
  border: 1px solid gray;
  padding: 0.5em;
  position: absolute;
  background: white;
  z-index: 9999;
}

.position__top_left {
  top: -100%;
  left: 0;
  transform: translate(-100%, -0.5em);
}

.position__top_center {
  top: -100%;
  left: 50%;
  transform: translate(-50%, -0.5em);
}

.position__top_right {
  top: -100%;
  right: 0;
  transform: translate(100%, -0.5em);
}

.position__left_top {
  top: 0;
  left: 0;
  transform: translateX(-100%);
}

.position__right_top {
  top: 0;
  left: 100%;
}

.position__left_center {
  top: 50%;
  right: 100%;
  transform: translateY(-50%);
}

.position__right_center {
  top: 50%;
  left: 100%;
  transform: translateY(-50%);
}

.position__left_bottom {
  bottom: 0;
  right: 100%;
}

.position__right_bottom {
  bottom: 0;
  left: 100%;
}

.position__bottom_left {
  top: 100%;
  right: 100%;
}

.position__bottom_center {
  bottom: 100;
  left: 50%;
  transform: translate(-50%, 0.5em);
}

.position__bottom_right {
  top: 100%;
  left: 100%;
}
</style>
