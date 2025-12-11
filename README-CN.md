# ecache - Emacs 通用缓存系统

一个功能强大、易于使用的 Emacs 缓存系统，灵感来自浏览器的 localStorage/sessionStorage 和其他编程语言的优秀缓存库。

## 特性

- **多种存储后端**
  - 内存缓存（全局和 buffer-local）
  - 文件缓存（持久化）
  - 数据库缓存（SQLite，Emacs 29+）

- **灵活的缓存策略**
  - TTL（Time To Live）过期时间
  - LRU（Least Recently Used）淘汰策略
  - 空闲时间自动失效
  - Hook 触发的缓存失效

- **简单的 API**
  - 类似 localStorage 的 key-value 接口
  - 自动序列化/反序列化
  - 支持任意 Lisp 数据类型

- **自动持久化**
  - 定期自动保存内存缓存到磁盘
  - 启动时自动加载

## 安装

### 手动安装

将 `ecache.el` 下载到你的 Emacs 配置目录，然后在 `init.el` 中添加：

```elisp
(add-to-list 'load-path "/path/to/ecache")
(require 'ecache)
(ecache-init)  ; 初始化缓存系统
```

### 配置

```elisp
;; 自定义缓存目录
(setq ecache-directory (expand-file-name "cache" user-emacs-directory))

;; 设置默认过期时间（秒）
(setq ecache-default-ttl 3600)  ; 1小时

;; 设置内存缓存最大容量
(setq ecache-max-memory-size 1000)

;; 设置空闲时间失效（秒）
(setq ecache-idle-invalidation-time 600)  ; 10分钟

;; 设置自动保存间隔（秒）
(setq ecache-auto-save-interval 300)  ; 5分钟
```

## 使用方法

### 基本操作

#### 内存缓存（全局）

```elisp
;; 设置缓存
(ecache-set "my-key" "my-value")

;; 获取缓存
(ecache-get "my-key")  ; => "my-value"

;; 获取缓存（带默认值）
(ecache-get "nonexistent-key" :default "default-value")  ; => "default-value"

;; 删除缓存
(ecache-delete "my-key")

;; 清空所有缓存
(ecache-clear)

;; 获取所有键
(ecache-keys)  ; => ("key1" "key2" ...)
```

#### 带 TTL 的缓存

```elisp
;; 设置60秒后过期的缓存
(ecache-set "temp-key" "temp-value" :ttl 60)

;; 60秒内有效
(ecache-get "temp-key")  ; => "temp-value"

;; 60秒后自动失效
(sleep-for 61)
(ecache-get "temp-key")  ; => nil
```

#### Buffer-Local 缓存

```elisp
;; 在当前 buffer 中设置缓存
(ecache-set "buffer-key" "buffer-value" :buffer-local t)

;; 从当前 buffer 获取缓存
(ecache-get "buffer-key" :buffer-local t)  ; => "buffer-value"

;; 切换到其他 buffer 后缓存不共享
(with-current-buffer other-buffer
  (ecache-get "buffer-key" :buffer-local t))  ; => nil
```

### 文件缓存

文件缓存会将数据持久化到磁盘，重启 Emacs 后数据依然存在。

```elisp
;; 设置文件缓存
(ecache-set "persistent-key" "persistent-value" :backend 'file)

;; 获取文件缓存
(ecache-get "persistent-key" :backend 'file)  ; => "persistent-value"

;; 删除文件缓存
(ecache-delete "persistent-key" :backend 'file)

;; 清空所有文件缓存
(ecache-clear :backend 'file)

;; 获取所有文件缓存的键
(ecache-keys :backend 'file)
```

### 数据库缓存（需要 Emacs 29+）

数据库缓存使用 SQLite 存储，适合大量数据的场景。

```elisp
;; 设置数据库缓存
(ecache-set "db-key" "db-value" :backend 'database)

;; 获取数据库缓存
(ecache-get "db-key" :backend 'database)  ; => "db-value"

;; 删除数据库缓存
(ecache-delete "db-key" :backend 'database)

;; 清空所有数据库缓存
(ecache-clear :backend 'database)

;; 获取所有数据库缓存的键
(ecache-keys :backend 'database)
```

### 支持的数据类型

ecache 支持任意可序列化的 Lisp 数据类型：

```elisp
;; 字符串
(ecache-set "string" "Hello, World!")

;; 数字
(ecache-set "number" 42)

;; 列表
(ecache-set "list" '(1 2 3 4 5))

;; 关联列表
(ecache-set "alist" '((name . "John") (age . 30)))

;; 属性列表
(ecache-set "plist" '(:name "John" :age 30))

;; 哈希表
(let ((hash (make-hash-table :test 'equal)))
  (puthash "key" "value" hash)
  (ecache-set "hash" hash))

;; 向量
(ecache-set "vector" [1 2 3 4 5])
```

## 高级用法

### 缓存失效策略

#### Hook 触发的缓存失效

```elisp
;; 在特定 hook 触发时清空缓存
(ecache-invalidate-on-hook 'after-save-hook 'memory)

;; 在 hook 触发时只删除特定的键
(ecache-invalidate-on-hook 'after-save-hook 'memory '("key1" "key2"))

;; 在文件保存后清空文件缓存
(ecache-invalidate-on-hook 'after-save-hook 'file)
```

#### 空闲时间失效

```elisp
;; 设置10分钟空闲后自动清空内存缓存
(setq ecache-idle-invalidation-time 600)
(ecache-invalidate-on-idle)
```

#### 自动保存

```elisp
;; 设置每5分钟自动保存内存缓存到文件
(setq ecache-auto-save-interval 300)
(ecache-setup-auto-save)
```

### 手动保存和加载

```elisp
;; 保存内存缓存到文件
(ecache-save-to-file)

;; 从文件加载内存缓存
(ecache-load-from-file)

;; 使用自定义文件路径
(ecache-save-to-file "/path/to/cache-backup.el")
(ecache-load-from-file "/path/to/cache-backup.el")
```

### 检查和调试

```elisp
;; 查看内存缓存内容
(ecache-inspect-memory)

;; 查看 buffer-local 缓存内容
(ecache-inspect-buffer-local)

;; 显示缓存统计信息
(ecache-stats)  ; => "Cache Statistics: Memory=10 File=5 Database=3"
```

## 实际应用场景

### 缓存 API 请求结果

```elisp
(defun my-fetch-data (url)
  "获取 URL 数据，使用缓存避免重复请求。"
  (let ((cache-key (format "api:%s" url)))
    (or (ecache-get cache-key :backend 'file)
        (let ((data (my-api-request url)))
          (ecache-set cache-key data :backend 'file :ttl 3600)
          data))))
```

### 缓存计算密集型结果

```elisp
(defun my-expensive-computation (input)
  "执行计算密集型操作，缓存结果。"
  (let ((cache-key (format "compute:%S" input)))
    (or (ecache-get cache-key)
        (let ((result (my-compute input)))
          (ecache-set cache-key result :ttl 600)
          result))))
```

### Buffer-Local 配置

```elisp
(defun my-buffer-config-set (key value)
  "设置 buffer-local 配置。"
  (ecache-set key value :buffer-local t))

(defun my-buffer-config-get (key &optional default)
  "获取 buffer-local 配置。"
  (ecache-get key :buffer-local t :default default))
```

### 项目级别的缓存

```elisp
(defun my-project-cache-set (key value)
  "设置项目级别的缓存。"
  (let ((project-root (project-root (project-current))))
    (ecache-set (format "%s:%s" project-root key) 
                value 
                :backend 'file)))

(defun my-project-cache-get (key)
  "获取项目级别的缓存。"
  (let ((project-root (project-root (project-current))))
    (ecache-get (format "%s:%s" project-root key)
                :backend 'file)))
```

## 性能考虑

### 选择合适的后端

- **内存缓存**: 最快，适合频繁访问的小数据
- **文件缓存**: 中等速度，适合需要持久化的中等大小数据
- **数据库缓存**: 适合大量数据，支持复杂查询（需要 Emacs 29+）

### LRU 淘汰策略

当内存缓存达到 `ecache-max-memory-size` 限制时，会自动淘汰最近最少使用的条目。

```elisp
;; 设置最大缓存条目数
(setq ecache-max-memory-size 1000)
```

## API 参考

### 核心函数

- `(ecache-set KEY VALUE &rest ARGS)` - 设置缓存
- `(ecache-get KEY &rest ARGS)` - 获取缓存
- `(ecache-delete KEY &rest ARGS)` - 删除缓存
- `(ecache-clear &rest ARGS)` - 清空缓存
- `(ecache-keys &rest ARGS)` - 获取所有键

### 参数

ARGS 是一个 plist，支持以下键：

- `:backend` - 后端类型：`'memory`（默认）、`'file` 或 `'database`
- `:ttl` - 过期时间（秒）
- `:default` - 默认值（仅用于 `ecache-get`）
- `:buffer-local` - 是否使用 buffer-local 缓存（覆盖 `:backend`）

### 特定后端函数

内存缓存：
- `ecache-set-memory`, `ecache-get-memory`, `ecache-delete-memory`
- `ecache-clear-memory`, `ecache-keys-memory`

Buffer-Local 缓存：
- `ecache-set-buffer-local`, `ecache-get-buffer-local`, `ecache-delete-buffer-local`
- `ecache-clear-buffer-local`, `ecache-keys-buffer-local`

文件缓存：
- `ecache-set-file`, `ecache-get-file`, `ecache-delete-file`
- `ecache-clear-file`, `ecache-keys-file`

数据库缓存：
- `ecache-set-database`, `ecache-get-database`, `ecache-delete-database`
- `ecache-clear-database`, `ecache-keys-database`

### 管理函数

- `(ecache-init)` - 初始化缓存系统
- `(ecache-save-to-file &optional FILE)` - 保存内存缓存到文件
- `(ecache-load-from-file &optional FILE)` - 从文件加载内存缓存
- `(ecache-invalidate-on-hook HOOK BACKEND &optional KEYS)` - 设置 hook 触发的失效
- `(ecache-invalidate-on-idle)` - 设置空闲失效
- `(ecache-setup-auto-save)` - 设置自动保存

### 交互式命令

- `M-x ecache-inspect-memory` - 检查内存缓存内容
- `M-x ecache-inspect-buffer-local` - 检查 buffer-local 缓存内容
- `M-x ecache-stats` - 显示缓存统计信息
- `M-x ecache-init` - 初始化缓存系统

## 设计理念

ecache 的设计借鉴了以下优秀实践：

1. **浏览器存储 API**: 简单的 key-value 接口，类似 localStorage/sessionStorage
2. **Redis**: 支持 TTL 和多种后端
3. **Python functools.lru_cache**: LRU 淘汰策略
4. **Memcached**: 分层缓存策略

## 常见问题

### 数据库缓存不可用？

数据库缓存需要 Emacs 29+ 和内置的 SQLite 支持。检查：

```elisp
(sqlite-available-p)  ; 应该返回 t
```

### 如何清空所有缓存？

```elisp
;; 清空所有后端的缓存
(ecache-clear :backend 'all)
```

### 缓存文件存储在哪里？

默认在 `~/.emacs.d/ecache/` 目录。可以通过 `ecache-directory` 自定义。

## 许可证

GPL-3.0 或更高版本

## 贡献

欢迎提交 issue 和 pull request！
