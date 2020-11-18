// Webpack
const webpack = require('webpack');

// Webpack plugins
const merge = require('webpack-merge');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const ExtractTextPlugin = require('extract-text-webpack-plugin');
const TerserPlugin = require('terser-webpack-plugin');
const SriPlugin = require('webpack-subresource-integrity');
const ZopfliPlugin = require('./vendor/zopfli-webpack-plugin').default;
const BrotliPlugin = require('brotli-webpack-plugin');
const HashOutput = require('./vendor/webpack-plugin-hash-output');
const OptimizeCssAssetsPlugin = require('optimize-css-assets-webpack-plugin');
const WorkboxWebpackPlugin = require('workbox-webpack-plugin');
const CopyPlugin = require('copy-webpack-plugin');

// Local stuff
const createHashFunction = require('./hash-func.js');

// Node helpers
const path = require('path');
const fs = require('fs');
const execSync = require('child_process').execSync;

// Constants
const entryPath = path.join(__dirname, 'src/static/index.js');
const outputPath = path.join(__dirname, 'dist');

// Webpack setup
// -------------
//
// Quick usage:
//
// - development: run "yarn start"
// - production: run "yarn build"
//
// More details:
//
// To do development, run "yarn start". This will serve the code
// locally at the designated port.  All Elm, JavaScript, and Sass
// files will be hot reloaded into the browser.  The standard URL
// for the development server is http://localhost:8000/.
//
// For production, run "yarn build".  This will create files that
// can be served by any static file web server.  While it's important
// to not cache index.html, all files in the static directory (CSS,
// images, fonts, and other resources) can be cached as they have
// hashed filenames.  For best results, it's recommended to put these
// files on a CDN.
//
// Options:
//
// --baseUrl=<url>     Set the base URL for service calls, etc.
// --production        Set the target environment to production.
// --development       Set the target environment to development.
// --languageSwitcher  Set if the languageSwitcher will be shown.
// --debug             Set the target environment to production with
//                     --the Elm debugger enabled.
//
// How to use options:
//
// <yarn command> -- <options>
// <webpack command> <options>
//
// Examples:
//
// yarn start -- --baseUrl=<url>
// yarn build -- --debug
//
// webpack --debug
// webpack-dev-server --hot --inline --baseUrl=<url>

// -- Helpers

const production = 'env-production';
const development = 'env-development';
const debug = 'env-debug';

// Try to load local config file, otherwise use default config.
function loadConfig() {
    try {
        const localConfig = require('./webpack.local.config');
        console.log('Loaded local config.');
        return localConfig;
    } catch (ex) {
        console.log('Loaded default config.');
        return {
            host: '0.0.0.0',
            port: 8000
        };
    }
}

// Get option from command line.
function getOption(optName, needsArgument, fallback) {
    const args = process.argv;

    if (needsArgument === true) {
        const fullOptName = '--' + optName + '=';

        for (let i = 0; i < process.argv.length; ++i) {
            if (args[i].substr(0, fullOptName.length) === fullOptName) {
                return args[i].substr(fullOptName.length);
            }
        }

        return fallback;
    } else {
        return args.indexOf('--' + optName) !== -1;
    }
}

// Get the target environment.
function getTargetEnv() {
    if (getOption('production')) {
        return production;
    } else if (getOption('development')) {
        return development;
    } else if (getOption('debug')) {
        return debug;
    } else if (process.env.npm_lifecycle_event === 'build') {
        return production;
    }

    return development;
}

// Get base URL from command line or config file.
function getBaseUrl(config) {
    const wantedBaseUrl = getOption('baseUrl', true, null);

    if (typeof wantedBaseUrl === 'string' && wantedBaseUrl.length > 0) {
        return wantedBaseUrl;
    }

    if (typeof config.baseUrl === 'string' && config.baseUrl.length > 0) {
        return config.baseUrl;
    }

    if (typeof config.baseUrl === 'object') {
        if (isProduction || isDebug) {
            if (typeof config.baseUrl.production === 'string' && config.baseUrl.production.length > 0) {
                return config.baseUrl.production;
            }
        } else if (isDevelopment) {
            if (typeof config.baseUrl.development === 'string' && config.baseUrl.development.length > 0) {
                return config.baseUrl.development;
            }
        }
    }

    // Use root of current server as default backend. This won't work for
    // development as the dev server only serves frontend code. It's just
    // a semi-sane default.
    return '/';
}

// Get languageSwitcher from command line or config file
function getLanguageSwitcher(config) {
    return getOption('languageSwitcher', true, config.languageSwitcher || false);
}

// Try to run a Git command. If Git isn't found in the PATH, then we
// silently return an empty string.
function runGitCommand(command) {
    try {
        return ('' + execSync(['git', command].join(' '))).replace(/[\s\r\n]+$/, '');
    } catch (ex) {
        return '';
    }
}

// Run "git describe".
function gitDescribe() {
    return runGitCommand('describe --always --tags');
}

// Return the current commit hash.
function gitCommitHash() {
    return runGitCommand('rev-parse HEAD');
}

// -- Webpack setup

console.log('Initializing...');

// Local config
const localConfig = loadConfig();
const publicPath = localConfig.publicPath || `${localConfig.host}:${localConfig.port}`;

// Determine build environment.
const TARGET_ENV = getTargetEnv();
const isProduction = TARGET_ENV === production || TARGET_ENV === debug;
const isDevelopment = TARGET_ENV === development;
const languageSwitcher = getLanguageSwitcher(localConfig);
const enableDebug = TARGET_ENV === development || TARGET_ENV === debug;

// Common Webpack config.  Everything here will be used in both the
// development and production environments.
const outputFilename = isDevelopment ? '[name].js' : '[chunkhash].js';
const commonConfig = {
    mode: isDevelopment ? 'development' : 'production',
    output: {
        path: outputPath,
        filename: `static/js/${outputFilename}`,
        crossOriginLoading: 'anonymous',
        publicPath: '',
        hashFunction: createHashFunction('sha256', 'base64'),
        hashDigestLength: 64
    },
    resolve: {
        modules: [path.join(__dirname, 'src'), 'node_modules'],
        extensions: ['.js', '.elm', '.scss']
    },
    module: {
        noParse: /\.elm$/
    },
    target: 'web',
    performance: {},
    optimization: {
        splitChunks: {
            name: 'vendor',
            minChunks: 2
        },
        noEmitOnErrors: true
    },
    plugins: [
        new SriPlugin({
            hashFuncNames: ['sha256', 'sha384'],
            enabled: !isDevelopment
        }),
        new HtmlWebpackPlugin({
            template: 'src/static/index.html',
            inject: true,
            filename: 'index.html',
            minify: isDevelopment ? {} : {
                removeAttributeQuotes: false,
                collapseWhitespace: true,
                html5: true,
                minifyCSS: true,
                removeComments: true,
                removeEmptyAttributes: true,
            }
        }),
        new CopyPlugin([
            {
                from: 'src/static/manifest.json',
                to: 'manifest.json',
                transform(content, path) {
                    return JSON.stringify(JSON.parse(content.toString('utf8')));
                }
            },
            {
                from: 'src/static/favicon.ico',
                to: 'favicon.ico'
            },
            {
                from: 'src/static/icon.svg',
                to: 'icon.svg'
            }
        ]),
        new webpack.DefinePlugin({
            elmFlags: JSON.stringify({
                isDevelopment: isDevelopment,
                baseUrl: getBaseUrl(localConfig),
                languageSwitcher: languageSwitcher || isDevelopment,
                version: gitDescribe(),
                commit: gitCommitHash()
            })
        })
    ]
};

// Finally, export the Webpack configuration.
if (isDevelopment) {
    console.log('Building for development...');
    console.log('Serving locally...');

    module.exports = merge(commonConfig, {
        entry: [
            `webpack-dev-server/client?http://${publicPath}`,
            entryPath
        ],
        devServer: {
            host: localConfig.host,
            port: localConfig.port,
            https: false,
            disableHostCheck: true,
            historyApiFallback: true,
            contentBase: './src',
            proxy: localConfig.proxy || {
                '/test/*': {
                    target: 'http://localhost:10000',
                    secure: false
                }
            }
        },
        module: {
            rules: [
                {
                    test: /\.elm$/,
                    exclude: [/elm-stuff/, /node_modules/],
                    use: [
                        {
                            loader: 'elm-hot-webpack-loader'
                        },
                        {
                            loader: 'elm-webpack-loader',
                            options: {
                                verbose: true,
                                debug: true
                            }
                        }
                    ]
                },
                {
                    test: /\.scss$/,
                    exclude: /node_modules/,
                    use: [
                        'style-loader',
                        'css-loader',
                        'postcss-loader',
                        'sass-loader'
                    ]
                },
                {
                    test: /\.css$/,
                    include: /node_modules/,
                    use: ['style-loader', 'css-loader']
                },
                {
                    test: /\.(svg|eot|otf|ttf|woff|woff2)$/,
                    use: 'file-loader'
                },
                {
                    test: /\.(jpg|png|gif)$/,
                    use: [
                        {
                            loader: 'url-loader',
                            options: {
                                limit: 10000
                            }
                        }
                    ]
                }
            ]
        }
    });
} else if (isProduction) {
    console.log('Building for production...');

    module.exports = merge(commonConfig, {
        entry: entryPath,
        module: {
            rules: [
                {
                    test: /\.elm$/,
                    exclude: [/elm-stuff/, /node_modules/],
                    use: {
                        loader: 'elm-webpack-loader',
                        options: {
                            debug: enableDebug,
                            optimize: !enableDebug
                        }
                    }
                },
                {
                    test: /\.(eot|otf|ttf|woff|woff2|jpe?g|png|gif|svg)$/i,
                    use: {
                        loader: 'file-loader',
                        options: {
                            name: 'static/res/[sha256:hash:base64].[ext]'
                        }
                    }
                },
                {
                    test: /\.(css|scss)$/,
                    use: ExtractTextPlugin.extract({
                        fallback: 'style-loader',
                        use: [
                            'css-loader',
                            'postcss-loader',
                            'sass-loader'
                        ],
                        publicPath: '../../'
                    })
                }
            ]
        },
        plugins: [
            new webpack.optimize.OccurrenceOrderPlugin(),

            // Minify JavaScript.
            new TerserPlugin({
                terserOptions: {
                    ecma: 5,
                    compress: {
                        pure_funcs: ['F2','F3','F4','F5','F6','F7','F8','F9','A2','A3','A4','A5','A6','A7','A8','A9'],
                        pure_getters: true,
                        keep_fargs: false,
                        unsafe_comps: true,
                        unsafe: true,
                        passes: 3
                    }
                }
            }),

            // Extract CSS into a separate file.
            new ExtractTextPlugin({
                filename: 'static/styles/[sha256:contenthash:base64].css',
                allChunks: true
            }),

            // Minify CSS.
            new OptimizeCssAssetsPlugin({
                cssProcessor: require('cssnano'),
                cssProcessorPluginOptions: {
                    preset: ['default', {discardComments: {removeAll: true}}],
                },
                canPrint: true
            }),

            // Hash output _after_ generating them
            new HashOutput({}),

            // Set up Workbox
            new WorkboxWebpackPlugin.GenerateSW(),

            // Compress everything with Zopfli (gzip)
            new ZopfliPlugin({
                asset: "[path].gz[query]",
                algorithm: "gzip",
                test: /\.js$|\.css$|\.html$|\.eot?.+$|\.ttf?.+$|\.woff?.+$|\.svg?.+$/,
                threshold: 1024,
                minRatio: 0.8
            }),

            // Compress everything with Brotli
            new BrotliPlugin({
                asset: '[path].br[query]',
                test: /\.js$|\.css$|\.html$|\.eot?.+$|\.ttf?.+$|\.woff?.+$|\.svg?.+$/,
                threshold: 1024,
                minRatio: 0.8
            }),
        ]
    });
} else {
    console.log('ERROR: Invalid target environment "' + TARGET_ENV + '".');
    module.exports = {};
}
