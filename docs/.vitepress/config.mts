import { withMermaid } from 'vitepress-plugin-mermaid'
import apiSidebar from '../api/_sidebar.json'

// one sidebar for every page but the API, in reading order: the "previous" and "next" links at the bottom of a page
// follow it, so the documentation reads from the first page to the last
const docs = [
  {
    text: 'Start here',
    items: [
      { text: 'Introduction', link: '/guide/' },
      { text: 'Installation', link: '/guide/install' },
    ],
  },
  {
    text: 'Tutorial',
    items: [
      { text: 'Overview',                   link: '/manual/' },
      { text: '1. A first quantity',        link: '/manual/tutorial/01-first-quantity' },
      { text: '2. Defining units',          link: '/manual/tutorial/02-defining-units' },
      { text: '3. The algebra of units',    link: '/manual/tutorial/03-unit-algebra' },
      { text: '4. What FURY refuses',       link: '/manual/tutorial/04-consistency' },
      { text: '5. Conversions',             link: '/manual/tutorial/05-conversions' },
      { text: '6. The SI system',           link: '/manual/tutorial/06-si-system' },
      { text: '7. Precision',               link: '/manual/tutorial/07-precision' },
      { text: '8. Non-linear conversions',  link: '/manual/tutorial/08-converters' },
      { text: '9. A units system of yours', link: '/manual/tutorial/09-own-system' },
    ],
  },
  {
    text: 'Recipes',
    items: [
      { text: 'Cookbook', link: '/manual/cookbook' },
    ],
  },
  {
    text: 'Reference',
    items: [
      { text: 'Feature map',   link: '/guide/features' },
      { text: 'Unit grammar',  link: '/guide/grammar' },
      { text: 'Units',         link: '/guide/units' },
      { text: 'Quantities',    link: '/guide/quantities' },
      { text: 'Conversions',   link: '/guide/conversions' },
      { text: 'Units systems', link: '/guide/systems' },
      { text: 'Precision',     link: '/guide/precision' },
      { text: 'Errors',        link: '/guide/errors' },
    ],
  },
  {
    text: 'Project',
    items: [
      { text: 'Background',   link: '/guide/background' },
      { text: 'Upgrading',    link: '/guide/migration' },
      { text: 'Changelog',    link: '/guide/changelog' },
      { text: 'Contributing', link: '/guide/contributing' },
    ],
  },
]

export default withMermaid({
  title: 'FURY',
  description: 'Fortran Units (environment) for Reliable phYsical math',
  base: '/FURY/',

  markdown: {
    math: true,
    languages: ['fortran-free-form', 'fortran-fixed-form'],
    languageAlias: {
      fortran: 'fortran-free-form',
      f90: 'fortran-free-form',
      f03: 'fortran-free-form',
      f08: 'fortran-free-form',
    },
  },

  themeConfig: {
    nav: [
      { text: 'Home', link: '/' },
      { text: 'Start here', link: '/guide/', activeMatch: '^/guide/(index|install)' },
      { text: 'Tutorial', link: '/manual/tutorial/01-first-quantity', activeMatch: '^/manual/(index|tutorial/)' },
      { text: 'Cookbook', link: '/manual/cookbook', activeMatch: '^/manual/cookbook' },
      {
        text: 'Reference',
        link: '/guide/features',
        activeMatch: '^/guide/(features|grammar|units|quantities|conversions|systems|precision|errors)',
      },
      { text: 'API', link: '/api/' },
      {
        text: 'Project',
        items: [
          { text: 'Background',   link: '/guide/background' },
          { text: 'Upgrading',    link: '/guide/migration' },
          { text: 'Changelog',    link: '/guide/changelog' },
          { text: 'Contributing', link: '/guide/contributing' },
        ],
      },
    ],

    sidebar: {
      '/guide/': docs,
      '/manual/': docs,
      '/api/': [
        {
          text: 'API Reference',
          items: [
            { text: 'Overview', link: '/api/' },
          ],
        },
        ...apiSidebar,
      ],
    },

    socialLinks: [
      { icon: 'github', link: 'https://github.com/szaghi/FURY' },
    ],

    search: {
      provider: 'local',
    },

    footer: {
      message: 'Released under GPL v3, BSD 2-Clause, BSD 3-Clause or MIT, at your choice.',
      copyright: 'Copyright © Stefano Zaghi',
    },
  },

  mermaid: {},

  vite: {
    // Build with an explicit modern JS target so the docs compile regardless of
    // which mermaid/vitepress/esbuild versions npm resolves. Vite's default
    // es2020 target forces esbuild to down-level modern syntax (e.g. the
    // destructuring mermaid 11.16+ emits), which it refuses to do and the build
    // dies. es2022 needs no lowering and is within VitePress's browser floor.
    build: {
      target: 'es2022',
    },
  },
})
