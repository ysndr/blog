const defaultTheme = require('tailwindcss/defaultTheme')
const colors = require('tailwindcss/colors')

module.exports = {
  purge: {
    content: [
    './src/**/*.html',
    ],
  },
  theme: {
    extend: {
      fontFamily: {
        sans: [
          'Fira Sans',
          defaultTheme.fontFamily.sans,
        ],
        // serif: [
        //   'Roboto Slab',
        //   defaultTheme.fontFamily.serif
        // ],
        'standout': [
            'Fira Code', 'monospace'
        ]
      },
      fontSize: {
        body: ["15px", "22.5px"]
      },
      colors: {
          muted: "#f5f5f5",
          emphasis: "#444",
          dark: "#222",

        // primary: 'var(--color-primary)',
        // secondary: 'var(--color-secondary)',
        // heading: 'var(--color-heading)',
        // body: 'var(--color-body)'
      },
      textColor: {
        muted: "#999",
        mutedDark: "#b5b5b5",
        emphasizedDark: "#fff",
        black: "#444"
      }
    },
  },
  variants: {},
  plugins: [],
}
