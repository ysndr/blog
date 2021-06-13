module.exports = {
  plugins: [
    require('postcss-import')({
      addModulesDirectories: [process.env["POSTCSS_MODULES"]]
    }),
    require('postcss-nested'),
    require('tailwindcss'),
    require('autoprefixer'),
  ]
}
