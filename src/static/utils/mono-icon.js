const darkmodeMedia = '(prefers-color-scheme: dark)';

class ImageThemed extends HTMLElement {
    connectedCallback() {
        const isDarkMode = window.matchMedia(darkmodeMedia).matches;
        this.img = document.createElement('img');
        this.img.setAttribute('data-original-src', this.src);
        this.img.src = renameFile(this.src, isDarkMode);
        this.img.alt = this.alt;
        for (let attr of this.attributes) {
            if (attr.name === 'class') continue;
            this.img.setAttribute(attr.name, attr.value);
        }

        this.appendChild(this.img);
        this.classList.add('ui-img-dark');
    }

    updateImageSource(isDarkMode) {
        this.img.src = renameFile(
            this.img.getAttribute('data-original-src'),
            isDarkMode
        );
    }
}

customElements.define('atb-mono-icon', ImageThemed);

const mql = window.matchMedia(darkmodeMedia);
updateAllThemedIcons(mql);
mql.addEventListener('change', updateAllThemedIcons);

function updateAllThemedIcons(e) {
    const elements = Array.from(document.querySelectorAll('atb-mono-icon'));
    elements.forEach(function (img) {
        img.updateImageSource(e.matches);
    });
}

function renameFile(filename, isDarkMode) {
    if (isDarkMode) {
        return filename.replace(/\/mono\//, '/mono/dark/');
    }
    return filename.replace(/\/mono\//, '/mono/light/');
}
