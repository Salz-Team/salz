<script lang="ts">
	import { Temporal } from 'temporal-polyfill';

	import FancyHeader from '$lib/components/FancyHeader.svelte';
	import { userStore } from '$lib/stores/user';
	import RoundInitial from '$lib/components/RoundInitial.svelte';
	import type { MatchResult as MatchResultModel } from '$lib/models';
	import MatchResult from '$lib/components/MatchResult.svelte';

	// Mocked bot results
	$: botResults = [
		{
			id: 'match111',
			timestamp: Temporal.Instant.from('2024-07-19T00:31:19Z'),
			bot1: {
				id: 'abc123',
				owner: {
					id: 'me1',
					username: 'John Smith',
				},
			},
			bot2: {
				id: 'bar312',
				owner: {
					id: 'foobar1',
					username: 'Foo Bar 1',
				},
			},
			winner: 'bot1',
		},
		{
			id: 'match110',
			timestamp: Temporal.Instant.from('2024-07-19T00:26:07Z'),
			bot1: {
				id: 'abc123',
				owner: {
					id: 'me1',
					username: 'John Smith',
				},
			},
			bot2: {
				id: 'bar314',
				owner: {
					id: 'foobar2',
					username: 'Foo Bar 2',
				},
			},
			winner: 'bot2',
		},
		{
			id: 'match109',
			timestamp: Temporal.Instant.from('2024-07-17T16:59:07Z'),
			bot1: {
				id: 'abc123',
				owner: {
					id: 'me1',
					username: 'John Smith',
				},
			},
			bot2: {
				id: 'bar311',
				owner: {
					id: 'foobar3',
					username: 'Foo Bar 3',
				},
			},
			winner: 'bot1',
		},
	] as MatchResultModel[];

	const localTimezoneId = Temporal.Now.timeZoneId();

	$: buckets = botResults.reduce(
		(acc, cur) => {
			const zonedDt = cur.timestamp.toZonedDateTimeISO(localTimezoneId);
			const localizedDate = zonedDt.toPlainDate().toLocaleString();
			if (Object.hasOwn(acc, localizedDate)) {
				acc[localizedDate] = [...acc[localizedDate], cur];
			} else {
				acc[localizedDate] = [cur];
			}

			return acc;
		},
		{} as { [key: string]: MatchResultModel[] },
	);
</script>

<main>
	<FancyHeader headerLevel="1">
		<RoundInitial slot="icon" name={$userStore?.username} />
		{$userStore?.username}
	</FancyHeader>

	<div class="content-container">
		<article class="my-match-results">
			<h2>Bot Results</h2>
			{#each Object.entries(buckets) as bucket}
				<section class="match-result-group">
					<h3>
						<span>{bucket[0]}</span>
					</h3>
					{#each bucket[1] as matchResult}
						<MatchResult
							{matchResult}
							datetimeDisplay={matchResult.timestamp
								.toZonedDateTimeISO(localTimezoneId)
								.toPlainTime()
								.toLocaleString()}
						/>
					{/each}
				</section>
			{/each}
		</article>
	</div>
</main>

<style>
	main {
		border: 0.1rem solid gray;
		padding: 0.3em 1.5em 1em 0em;
		border-radius: 5px 5px 10px 10px;
		min-height: 30vh;
		width: 60vw;
		margin: 2em auto 5em auto;
		background: white;
	}

	main :global(.fancy-header) {
		max-width: 60%;
	}

	main :global(.fancy-header .person-initials) {
		width: 1em;
	}

	.content-container {
		padding-top: 1.5em;
		padding-left: 1.5em;
	}

	.match-result-group + .match-result-group {
		margin-top: 2em;
	}

	.match-result-group > h3 {
		position: relative;
		padding-left: 1em;
		font-size: 1.6rem;
	}

	.match-result-group > h3::before {
		content: '';
		position: absolute;
		top: 50%;
		left: 0;
		width: 100%;
		height: 0.3rem;
		background: black;
	}

	.match-result-group > h3 span {
		position: relative;
		padding: 0 1em;
		background: white;
		z-index: 1;
	}
</style>
