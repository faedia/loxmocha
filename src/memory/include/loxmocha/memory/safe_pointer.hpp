#pragma once

#include <exception>
#include <memory>

namespace loxmocha {

class bad_nullable_ptr : public std::exception {
public:
    bad_nullable_ptr()                            = default;
    bad_nullable_ptr(const bad_nullable_ptr&)     = default;
    bad_nullable_ptr(bad_nullable_ptr&&) noexcept = default;

    auto operator=(const bad_nullable_ptr&) -> bad_nullable_ptr&     = default;
    auto operator=(bad_nullable_ptr&&) noexcept -> bad_nullable_ptr& = default;

    [[nodiscard]] auto what() const noexcept -> const char* override
    {
        return "bad nullable_ptr access: pointer is null";
    }
    ~bad_nullable_ptr() override = default;
};

template<typename T>
class safe_ptr;

template<typename T>
class nullable_ptr {
public:
    nullable_ptr() : ptr_(nullptr) {}
    explicit nullable_ptr(T* p) : ptr_(p) {}
    explicit nullable_ptr(std::nullptr_t) : ptr_(nullptr) {}

    [[nodiscard]] auto operator->() -> T* { return ptr_.get(); }
    [[nodiscard]] auto operator*() -> T& { return *ptr_; }
    [[nodiscard]] auto operator->() const -> const T* { return ptr_.get(); }
    [[nodiscard]] auto operator*() const -> const T& { return *ptr_; }

    [[nodiscard]] auto get() -> T* { return ptr_.get(); }
    [[nodiscard]] auto get() const -> const T* { return ptr_.get(); }

    explicit operator bool() const { return static_cast<bool>(ptr_); }

    void reset() { ptr_.reset(); }
    void reset(T* p) { ptr_.reset(p); }

    [[nodiscard]] auto to_safe() && -> safe_ptr<T>
    {
        if (ptr_) {
            return safe_ptr<T>(std::move(ptr_));
        }
        throw bad_nullable_ptr();
    }

    auto operator==(const nullable_ptr<T>& other) const -> bool { return ptr_ == other.ptr_; }
    auto operator==(std::nullptr_t) const -> bool { return ptr_ == nullptr; }

private:
    std::unique_ptr<T, std::default_delete<T>> ptr_;
};

template<typename T>
class safe_ptr {
public:
    safe_ptr()  = delete;
    ~safe_ptr() = default;

    safe_ptr(const safe_ptr& p)                    = delete;
    safe_ptr(safe_ptr&&)                           = default;
    auto operator=(const safe_ptr& p) -> safe_ptr& = delete;
    auto operator=(safe_ptr&&) -> safe_ptr&        = default;

    [[nodiscard]] auto operator->() -> T* { return ptr_.get(); }
    [[nodiscard]] auto operator*() -> T& { return *ptr_; }
    [[nodiscard]] auto operator->() const -> const T* { return ptr_.get(); }
    [[nodiscard]] auto operator*() const -> const T& { return *ptr_; }
    [[nodiscard]] auto get() -> T* { return ptr_.get(); }
    [[nodiscard]] auto get() const -> const T* { return ptr_.get(); }

    [[nodiscard]] auto to_nullable() && -> nullable_ptr<T> { return nullable_ptr<T>(ptr_.release()); }

    template<typename... Args>
    [[nodiscard]] static auto make(Args&&... args) -> safe_ptr<T>
    {
        auto ptr = std::make_unique<T>(std::forward<Args>(args)...);
        return safe_ptr<T>(std::move(ptr));
    }

private:
    std::unique_ptr<T, std::default_delete<T>> ptr_;

    // Allow nullable_ptr to access the private constructor
    friend class nullable_ptr<T>;
    explicit safe_ptr(std::unique_ptr<T>&& p) : ptr_(std::move(p)) {}
};

} // namespace loxmocha
