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
        serif: [
          "Merriweather",
          "Roboto Slab",
          defaultTheme.fontFamily.serif
        ],
        'standout': [
            'Fira Code', 'monospace'
        ]
      },
      fontSize: {
        '2xs': ".5rem",
        'xs': '.75rem',
        'sm': '.875rem',
        body: ["15px", "22.5px"],
        'lg': '1.125rem',
        'xl': '1.25rem',
        '2xl': '1.375rem',
       '3xl': '1.625rem',
       '4xl': '2.25rem',
        '5xl': '3rem',
        '6xl': '4rem',
       '7xl': '5rem',

        'lead': '1.125rem'
      },
      colors: {
          muted: "#f5f5f5",
          emphasis: "#444",
          dark: "#252525",
          hr: "#e5e5e5",
          code: "var(--bg-code)",
          highlight: "#5d8c91",

        // primary: 'var(--color-primary)',
        // secondary: 'var(--color-secondary)',
        // heading: 'var(--color-heading)',
        // body: 'var(--color-body)'
      },
      textColor: {
        muted: "#999",
        mutedDark: "#b5b5b5",
        emphasizedDark: "#fff",
        black: "#252525",
        code: "#3d545b",
      },
      container: {
        center: true,
        padding: {
          DEFAULT: '1rem',
          sm: '2rem',
          lg: '3rem',
          xl: '4rem',
          '2xl': '5rem',
        },
      },
      height: {
        xl: '32rem',
        '2xl': '40rem',
        '3xl': '48rem',
       },
    },

  },
  variants: {},
  plugins: [],
}
