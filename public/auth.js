const register = async () => {
    try {
        const startRegisterOptions = makeStartRegisterOptions()
        const publicKeyCredentialCreationOptions = await startRegister(startRegisterOptions)
        console.log(publicKeyCredentialCreationOptions)
        const credentials = await makeCredentials(publicKeyCredentialCreationOptions)
        const finishRegisterOptions = makeFinishRegisterOptions(credentials)
        console.log(finishRegisterOptions)
        const res = await finishRegister(finishRegisterOptions)
        console.log(res)
    } catch (err) {
        console.error(err.message)
        debug(err.message)
    }
}

const startRegister = startRegisterOptions =>
    request("/register/start", startRegisterOptions)

const finishRegister = finishRequestOptions =>
    request("/register/finish", finishRequestOptions)

const makeCredentials = options =>
    navigator.credentials.create({
        publicKey: makePublicKeyCredentialCreationOptions({
            ...options,
            pubKeyCredParams: [
                {
                    type: "public-key",
                    alg: -8,
                },
                {
                    type: "public-key",
                    alg: -7,
                },
                {
                    type: "public-key",
                    alg: -257,
                }
            ],
        })
    })

const makeStartRegisterOptions = () => ({
    login: document.getElementById("login").value,
    name: document.getElementById("name").value,
})

const makeFinishRegisterOptions = credentials => ({
    id: toBase64(credentials.rawId),
    challenge: fromBase64URL(getChallenge(credentials.response.clientDataJSON)),
    publicKey: toBase64(credentials.response.getPublicKey()),
    publicKeyAlgorithm: credentials.response.getPublicKeyAlgorithm(),
})

const makePublicKeyCredentialCreationOptions = options => ({
    ...options,
    user: {
        ...options.user,
        id: fromBase64(options.user.id),
    },
    challenge: fromBase64(options.challenge),
})

const getChallenge = (clientDataJSON) => {
    console.log(clientDataJSON)
    const decoder = new TextDecoder()
    return JSON.parse(decoder.decode(clientDataJSON)).challenge
}

const authenticate = () => {
    console.log("authenticate")
}

const request = async (url, data) => {
    const response = await fetch(url, {
        method: "POST",
        headers: {
            "Content-Type": "application/json",
        },
        body: JSON.stringify(data),
    })
    if (response.ok) {
        return response.json()
    }
    throw new Error(await response.text())
}

const fromBase64 = base64 =>
    Uint8Array.from(atob(base64), c => c.charCodeAt(0))

const toBase64 = data =>
    btoa(String.fromCharCode(...new Uint8Array(data)))

fromBase64URL = (data) => {
    const input = data
        .replace(/-/g, "+")
        .replace(/_/g, "/")
    const pad = input.length % 4
    if (pad) {
        if (pad === 1) {
            throw new Error("InvalidLengthError: Input base64url string is the wrong length to determine padding")
        }
        return input.padEnd(input.length + (4 - pad), "=")
    }
    return input
}

const debug = data => {
    const el = document.getElementById("debug")
    el.innerHTML =
        typeof data === "string"
            ? data
            : JSON.stringify(data, null, 4)
}
